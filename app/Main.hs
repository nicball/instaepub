{-# LANGUAGE OverloadedStrings, BlockArguments, QuasiQuotes, TemplateHaskell, LambdaCase #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, modifyMVar_, readMVar)
import Control.Exception (throwIO, handle)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isLetter, isSpace, isNumber)
import Data.FileEmbed (embedFile)
import Data.Functor (void)
import Data.IntMap.Strict qualified as Map
import Data.String.Interpolate (__i)
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as TextL
import Data.Text qualified as Text
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Exception (Exception)
import GHC.IO.Unsafe (unsafePerformIO)
import Network.HTTP.Client (parseRequest, httpLbs, responseBody, HttpException)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (status404, status403)
import System.Directory (removeFile)
import System.Environment (getEnv)
import Text.Pandoc (def, readHtml, docTitle, Pandoc(Pandoc), writeEPUB3, ReaderOptions (readerStandalone), WriterOptions (writerTemplate), compileDefaultTemplate, runIO, PandocError)
import Text.Pandoc.Shared (stringify)
import Web.Scotty (scotty, get, post, queryParam, formParam, pathParam, setHeader, json, html, text, raw, regex, redirect303, next, status)

{-# NOINLINE hostName #-}
hostName :: Text
hostName = Text.pack . unsafePerformIO . getEnv $ "HOSTNAME"

main :: IO ()
main = do
  jobs <- newJobs
  scotty 8086 do
    post "/jobs" do
      url <- formParam "url"
      jid <- liftIO . newJob $ jobs
      liftIO . void . forkIO . saveUrl jobs jid $ url
      redirect303 $ "/jobs/" <> TextL.pack (show jid)
    get (regex "^/jobs/([0-9]+)") do
      jid <- pathParam "1"
      let body st = text $ "Job ID: " <> TextL.pack (show jid) <> "\nStatus: " <> TextL.pack (show st)
      liftIO (queryJob jobs jid) >>= maybe next body
    get "/deleteJob" do
      jid <- queryParam "id"
      liftIO (queryJob jobs jid) >>= \case
        Nothing -> status status404 >> text "Job not found."
        Just (Completed fname) -> do
          liftIO . removeFile . Text.unpack $ fname
          liftIO . deleteJob jobs $ jid
          text "Done!"
        _ -> status status403 >> text "File not generated or already deleted."
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
    get (regex "^/(index.html)?$") . html $ [__i|
      <!DOCTYPE html>
      <html>
        <head>
          <link rel="manifest" href="/appmanifest" crossorigin="use-credentials" />
          <title>InstaEpub</title>
        </head>
        <body>
          <form action="/jobs" method="post" enctype="multipart/form-data">
            <input type="url" name="url" />
            <input type="submit" value="Read it later!" />
          </form>
        </body>
      </html>
    |]
    get "/icon.png" do
      setHeader "Content-Type" "image/png"
      raw (LBS.fromStrict $(embedFile "icon.png"))
    get "/screenshot.jpg" do
      setHeader "Content-Type" "image/jpeg"
      raw (LBS.fromStrict $(embedFile "screenshot.jpg"))

data JobStatus
  = Pending
  | forall e. Exception e => Failed e
  | Completed Text
  | Deleted

deriving instance Show JobStatus

data Jobs = Jobs (MVar (Map.IntMap JobStatus))

newJobs :: IO Jobs
newJobs = Jobs <$> newMVar Map.empty

newJob :: Jobs -> IO Int
newJob (Jobs jobs) = modifyMVar jobs \m -> do
  let i = (+ 1) . maybe 0 fst . Map.lookupMax $ m
  let m' = Map.insert i Pending m
  pure (m', i + 1)

completeJob :: Jobs -> Int -> Text -> IO ()
completeJob (Jobs jobs) i path = modifyMVar_ jobs $ pure . Map.insert i (Completed path)

deleteJob :: Jobs -> Int -> IO ()
deleteJob (Jobs jobs) i = modifyMVar_ jobs $ pure . Map.insert i Deleted

failJob :: Exception e => Jobs -> Int -> e -> IO ()
failJob (Jobs jobs) i e = modifyMVar_ jobs $ pure . Map.insert i (Failed e)

queryJob :: Jobs -> Int -> IO (Maybe JobStatus)
queryJob (Jobs jobs) i = (Map.!? i) <$> readMVar jobs

saveUrl :: Jobs -> Int -> Text -> IO ()
saveUrl jobs jid url = handleHttpException . handlePandocError $ do
  html' <- fetchHtml url
  (title, epub) <- liftEither =<< runIO do
    tmpl <- compileDefaultTemplate "epub3"
    del <- readHtml def $ "<p><a href=\"https://" <> hostName <> "/deleteJob?id=" <> Text.pack (show jid) <> "\">Delete this document</a></p>"
    doc@(Pandoc meta _) <- readHtml def { readerStandalone = True } html'
    (stringify (docTitle meta), ) <$> writeEPUB3 def { writerTemplate = Just tmpl } (del <> doc <> del)
  Text.putStrLn $ "Fetched article: " <> title
  time <- Text.pack . show <$> getPOSIXTime
  let fname = time <> "." <> sanitizeFileName title <> ".epub"
  LBS.writeFile (Text.unpack fname) epub
  completeJob jobs jid fname
  where
  handleHttpException = handle \(e :: HttpException) -> failJob jobs jid e
  handlePandocError = handle \(e :: PandocError) -> failJob jobs jid e

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
  liftEither . Text.decodeUtf8' . LBS.toStrict . responseBody $ resp

liftEither :: Exception e => Either e a -> IO a
liftEither (Left e) = throwIO e
liftEither (Right a) = pure a
