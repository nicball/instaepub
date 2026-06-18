{-# LANGUAGE OverloadedStrings
           , BlockArguments
           , QuasiQuotes
           , TemplateHaskell
           , LambdaCase
           , OverloadedRecordDot #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Exception (throwIO, handle)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isLetter, isSpace, isNumber)
import Data.FileEmbed (embedFile)
import Data.Functor (void)
import Data.Maybe (fromJust)
import Data.Text.Encoding.Error qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as TextL
import Data.Text qualified as Text
import Data.Text (Text)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)
import Data.Time.Format (formatTime, defaultTimeLocale, rfc822DateFormat)
import GHC.Exception (Exception)
import Network.HTTP.Client (parseRequest, httpLbs, responseBody, HttpException)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (status400)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (shamlet)
import Text.Pandoc (def, readHtml, docTitle, Pandoc(Pandoc), writeEPUB3, ReaderOptions(readerStandalone), WriterOptions(writerTemplate), compileDefaultTemplate, runIO, PandocError, Inline(Link, Image))
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Walk (Walkable(walk))
import Text.Regex.TDFA ((=~), getAllTextMatches)
import Text.URI qualified as URI
import Web.Scotty (scottyOpts, get, post, formParam, pathParam, setHeader, json, html, text, raw, regex, redirect303, next, finish, status, Parsable(..), readEither, Options(..), defaultOptions)
import Network.Wai.Handler.Warp (setLogger, setPort)

import Persist (JobID, Jobs, Status(..), Job(..), withJobs, newJob, doneJob, failJob, queryJob, getJobs, getPendingJobs)

main :: IO ()
main = withJobs "./instaepub.sqlite" \jobs -> do
  restartPendingJobs jobs
  let logRequest req st _ = putStrLn $ show req <> " " <> show st
  let options = defaultOptions { settings = setPort 8086 . setLogger logRequest . settings $ defaultOptions }
  scottyOpts options do
    post "/jobs" do
      url <- detectUrl <$> formParam "url" >>= \case
        Nothing -> do
          status status400
          text "Coundn't find any URLs."
          finish
        Just x -> pure x
      jid <- liftIO . newJob jobs $ url
      liftIO . void . forkIO . saveUrl jobs jid $ url
      redirect303 $ "/jobs"
    get "/jobs/:jid/epub" do
      jid <- parseJobID <$> pathParam "jid"
      liftIO (queryJob jobs jid) >>= \case
        Nothing -> next
        Just job -> case job.status of
          Done title getEpub -> do
            setHeader "Content-Type" "application/epub+zip"
            setHeader "Content-Disposition" $ "attachment; filename=\"" <> TextL.fromStrict (sanitizeFileName title) <> ".epub\""
            raw . LBS.fromStrict =<< liftIO getEpub
          _ -> next
    get "/jobs/:jid/errorLog" do
      jid <- parseJobID <$> pathParam "jid"
      liftIO (queryJob jobs jid) >>= \case
        Nothing -> next
        Just job -> case job.status of
          Failed msg -> text . TextL.fromStrict $ msg
          _ -> next
    get "/jobs" do
      topJobs <- liftIO $ getJobs jobs
      tz <- liftIO getCurrentTimeZone
      let shorten t = if Text.length t > 50 then Text.take 47 t <> "..." else t
      html . renderHtml $ [shamlet|
        $doctype 5
        <html>
          <head>
            <title>Jobs - InstaEpub
          <body>
            <table>
              <tr>
                <th>Added
                <th>URL
                <th>Title
                <th>Status
              $forall job <- topJobs
                <tr>
                  <td>#{formatTime defaultTimeLocale rfc822DateFormat (utcToLocalTime tz job.timeStamp)}
                  <td>
                    <a href="#{job.url}">
                      #{shorten job.url}
                  $case job.status
                    $of Pending
                      <td>N/A
                      <td>Pending
                    $of Done title _
                      <td>#{title}
                      <td>
                        <a href="/jobs/#{show job.id}/epub">
                          Completed
                    $of Failed _
                      <td>N/A
                      <td>
                        <a href="/jobs/#{show job.id}/errorLog">
                          Failed
      |]
    get "/appmanifest" do
      setHeader "Content-Type" "application/manifest+json"
      json $ [aesonQQ| {
        "name": "InstaEpub",
        "icons": [ {
          "src": "/icon.png",
          "type": "image/png",
          "sizes": "512x512"
        } ],
        "start_url": "/",
        "display": "standalone",
        "share_target": {
          "action": "/jobs",
          "method": "POST",
          "enctype": "multipart/form-data",
          "params": {
            "title": "title",
            "text": "url",
            "url": "link",
            "files": []
          }
        },
        "screenshots": [ {
          "src": "/screenshot.jpg",
          "sizes": "600x600",
          "type": "image/jpeg"
        } ]
      } |]
    get (regex "^/(index.html)?$") . html . renderHtml $ [shamlet|
      $doctype 5
      <html>
        <head>
          <link rel="manifest" href="/appmanifest" crossorigin="use-credentials">
          <title>InstaEpub
        <body>
          <form action="/jobs" method="post" enctype="multipart/form-data">
            <input type="url" name="url">
            <input type="submit" value="Read it later!">
    |]
    get "/icon.png" do
      setHeader "Content-Type" "image/png"
      raw (LBS.fromStrict $(embedFile "icon.png"))
    get "/screenshot.jpg" do
      setHeader "Content-Type" "image/jpeg"
      raw (LBS.fromStrict $(embedFile "screenshot.jpg"))

restartPendingJobs :: Jobs -> IO ()
restartPendingJobs jobs = do
  getPendingJobs jobs >>= mapM_ \job -> do
    Text.putStrLn $ "Restarting " <> job.url
    void . forkIO . saveUrl jobs job.id $ job.url

saveUrl :: Jobs -> JobID -> Text -> IO ()
saveUrl jobs jid url = handleHttpException . handlePandocError $ do
  html' <- fetchHtml url
  base <- URI.mkURI url
  (title, epub) <- liftEither =<< runIO do
    tmpl <- compileDefaultTemplate "epub3"
    doc@(Pandoc meta _) <- addBaseToLinks base <$> readHtml def { readerStandalone = True } html'
    (stringify (docTitle meta), ) <$> writeEPUB3 def { writerTemplate = Just tmpl } doc
  Text.putStrLn $ "Fetched article: " <> title
  doneJob jobs jid title (LBS.toStrict epub)
  where
  handleHttpException = handle \(e :: HttpException) -> failJob jobs jid . Text.pack . show $ e
  handlePandocError = handle \(e :: PandocError) -> failJob jobs jid . Text.pack . show $ e
  addBaseToLinks base = walk \case
    link@(Link attr alts (href, title)) ->
      case URI.mkURI href of
        Nothing -> link
        Just u -> Link attr alts (URI.render . fromJust $ u `URI.relativeTo` base, title)
    image@(Image attr alts (href, title))->
      case URI.mkURI href of
        Nothing -> image
        Just u -> Image attr alts (URI.render . fromJust $ u `URI.relativeTo` base, title)
    inline -> inline

detectUrl :: Text -> Maybe Text
detectUrl t = longest . getAllTextMatches $ t =~ url
  where
  url :: Text
  url = "(((http|https|Http|Https)://(([-a-zA-Z0-9$_.+!*'()"
      <> ",;?&=]|(%[a-fA-F0-9]{2})){1,64}(:([-a-zA-Z0-9$_"
      <> ".+!*'(),;?&=]|(%[a-fA-F0-9]{2})){1,25})?@)?)?"
      <> "(" <> domainName <> ")"
      <> "(:[[:digit:]]{1,5})?)" -- plus option port number
      <> "(/(([-" <> goodIriChar <> ";/?:@&=#~"  -- plus option query params
      <> ".+!*'(),_])|(%[a-fA-F0-9]{2}))*)?"
  domainName = "(" <> hostName' <> "|" <> ipAddress <> ")"
  hostName' = "(" <> iri <> "\\.)+" <> gtld
  iri = "[" <> goodIriChar <> "]([-" <> goodIriChar <> "]{0,61}[" <> goodIriChar <> "]){0,1}"
  gtld = "[" <> goodGtldChar <> "]{2,63}"
  goodGtldChar = "a-zA-Z\x00A0-\xD7FF\xF900-\xFDCF\xFDF0-\xFFEF"
  goodIriChar = "a-zA-Z0-9\x00A0-\xD7FF\xF900-\xFDCF\xFDF0-\xFFEF"
  ipAddress = "((25[0-5]|2[0-4][0-9]|[0-1][0-9]{2}|[1-9][0-9]|[1-9])\\.(25[0-5]|2[0-4]"
    <> "[0-9]|[0-1][0-9]{2}|[1-9][0-9]|[1-9]|0)\\.(25[0-5]|2[0-4][0-9]|[0-1]"
    <> "[0-9]{2}|[1-9][0-9]|[1-9]|0)\\.(25[0-5]|2[0-4][0-9]|[0-1][0-9]{2}"
    <> "|[1-9][0-9]|[0-9]))"
  longest :: [Text] -> Maybe Text
  longest xs = foldl' longer Nothing xs
  longer Nothing x = Just x
  longer (Just x) y
    | Text.length x <= Text.length y = Just y
    | otherwise = Just x

sanitizeFileName :: Text -> Text
sanitizeFileName = Text.map \c ->
  if isLetter c || isSpace c || isNumber c
    then c
    else '_'

fetchHtml :: Text -> IO Text
fetchHtml url = do
  man <- newTlsManager
  req <- parseRequest (Text.unpack url)
  resp <- httpLbs req man
  pure . Text.decodeUtf8With Text.lenientDecode . LBS.toStrict . responseBody $ resp

liftEither :: Exception e => Either e a -> IO a
liftEither (Left e) = throwIO e
liftEither (Right a) = pure a

newtype ParseJobID = ParseJobID { parseJobID :: JobID }
  deriving newtype Read
instance Parsable ParseJobID where
  parseParam = readEither
