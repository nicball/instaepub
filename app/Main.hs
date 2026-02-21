{-# LANGUAGE OverloadedStrings, BlockArguments, QuasiQuotes, TemplateHaskell #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isLetter, isSpace, isNumber)
import Data.FileEmbed (embedFile)
import Data.Functor (void)
import Data.String.Interpolate (__i)
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Text qualified as Text
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client (parseRequest, httpLbs, responseBody)
import Network.HTTP.Client.TLS (newTlsManager)
import Text.Pandoc (def, runIOorExplode, readHtml, docTitle, Pandoc(Pandoc), writeEPUB3, ReaderOptions (readerStandalone), WriterOptions (writerTemplate), compileDefaultTemplate)
import Text.Pandoc.Shared (stringify)
import Web.Scotty (scotty, get, post, formParam, setHeader, json, html, raw, regex, redirect)

main :: IO ()
main = scotty 8086 do
  post "/save" do
    url <- formParam "url"
    liftIO . void . forkIO $ saveUrl url
    redirect "https://nicball.online/instaepub"
  get "/appmanifest" do
    setHeader "Content-Type" "application/manifest+json"
    json $ [aesonQQ| {
      "name": "InstaEpub",
      "icons": [ {
        "src": "icon.png",
        "type": "image/png",
        "sizes": "512x512"
      } ],
      "start_url": "/",
      "display": "standalone",
      "share_target": {
        "action": "/save",
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
        "src": "screenshot.jpg",
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
        <form action="/save" method="post" enctype="multipart/form-data">
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

saveUrl :: Text -> IO ()
saveUrl url = do
  html' <- fetchHtml url
  (title, epub) <- runIOorExplode do
    tmpl <- compileDefaultTemplate "epub3"
    doc@(Pandoc meta _) <- readHtml def { readerStandalone = True } html'
    (stringify (docTitle meta), ) <$> writeEPUB3 def { writerTemplate = Just tmpl } doc
  Text.putStrLn $ "Fetched article: " <> title
  time <- show <$> getPOSIXTime
  LBS.writeFile (Text.unpack (sanitizeFileName title) <> "." <> time <> ".epub") epub

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
  where
    liftEither (Left e) = throwIO e
    liftEither (Right a) = pure a
