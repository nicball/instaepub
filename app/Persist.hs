{-# LANGUAGE QuasiQuotes
           , TemplateHaskell
           , TypeFamilies
           , UndecidableInstances
           , BlockArguments
           , OverloadedRecordDot
           , NoFieldSelectors
           , OverloadedStrings
           #-}

module Persist
  ( Jobs
  , Job(..)
  , Status(..)
  , JobID
  , newJobs
  , closeJobs
  , withJobs
  , newJob
  , doneJob
  , failJob
  , queryJob
  , getJobs
  , getPendingJobs
  ) where

import Control.Arrow (first)
import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.Logger (runStderrLoggingT, filterLogger, LogLevel(..))
import Database.Persist (PersistEntity(Key), PersistStoreRead(get), getBy, PersistStoreWrite(update), PersistUniqueWrite(upsertBy, upsert), (=.), selectList, SelectOpt(Desc), Entity(Entity, entityKey), (==.))
import Database.Persist.Sqlite (createSqlitePool)
import Database.Persist.Sql (runMigration, SqlBackend, runSqlPersistMPool)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.Pool (Pool, destroyAllResources)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)

import Persist.Types (JobStatusE(..))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
JobE sql=job
  Id Int default=NULL
  url Text
  timeStamp UTCTime
  status JobStatusE default='PendingE'
  errorLog Text Maybe default=NULL
  title Text Maybe default=NULL
  UniqueUrl url
EpubE sql=epub
  job JobEId OnDeleteCascade
  content ByteString
  Primary job
|]

data Job = Job
  { id :: JobID
  , url :: Text
  , timeStamp :: UTCTime
  , status :: Status
  }

data Status
  = Pending
  | Failed Text
  | Done Text (IO ByteString)

data Jobs = Jobs
  { pool :: Pool SqlBackend
  }

newtype JobID = JobID { key :: Key JobE }

instance Show JobID where
  show jid = show jid.key.unJobEKey

instance Read JobID where
  readsPrec p s = map (first (JobID . JobEKey)) (readsPrec p s)

newJobs :: Text -> IO Jobs
newJobs path = do
  pool <- runStderrLoggingT . filterLogger warning . createSqlitePool path $ 5
  runSqlPersistMPool (runMigration migrateAll) pool
  pure . Jobs $ pool
  where
    warning _ level = level >= LevelWarn

closeJobs :: Jobs -> IO ()
closeJobs jobs = destroyAllResources jobs.pool

withJobs :: Text -> (Jobs -> IO a) -> IO a
withJobs path = bracket (newJobs path) closeJobs

newJob :: Jobs -> Text -> IO JobID
newJob jobs url = do
  timeStamp <- getCurrentTime
  JobID <$> flip runSqlPersistMPool jobs.pool do
    entityKey <$> upsertBy (UniqueUrl url) (JobE url timeStamp PendingE Nothing Nothing) [JobETimeStamp =. timeStamp]

doneJob :: Jobs -> JobID -> Text -> ByteString -> IO ()
doneJob jobs jid title epub = do
  flip runSqlPersistMPool jobs.pool do
    void $ upsert (EpubE jid.key epub) [EpubEContent =. epub]
    update jid.key [JobEStatus =. DoneE, JobETitle =. Just title]

failJob :: Jobs -> JobID -> Text -> IO ()
failJob jobs jid msg = do
  flip runSqlPersistMPool jobs.pool do
    update jid.key [JobEStatus =. FailedE, JobEErrorLog =. Just msg]

queryJob :: Jobs -> JobID -> IO (Maybe Job)
queryJob jobs jid = fmap (convertJobE jobs . Entity jid.key) <$> runSqlPersistMPool (get jid.key) jobs.pool

convertJobE :: Jobs -> Entity JobE -> Job
convertJobE jobs (Entity jid (JobE url ts st mel mtitle)) = Job (JobID jid) url ts (convertStatus st mel mtitle)
  where
    convertStatus PendingE _ _ = Pending
    convertStatus DoneE _ (Just title) = Done title do
      flip runSqlPersistMPool jobs.pool do
        Entity _ epub <- fromJust <$> getBy (EpubEPrimaryKey jid)
        pure epub.epubEContent
    convertStatus FailedE (Just msg) _ = Failed msg
    convertStatus _ _ _ = undefined

getJobs :: Jobs -> IO [Job]
getJobs jobs = do
  flip runSqlPersistMPool jobs.pool do
    map (convertJobE jobs) <$> selectList [] [Desc JobETimeStamp]

getPendingJobs :: Jobs -> IO [Job]
getPendingJobs jobs = do
  flip runSqlPersistMPool jobs.pool do
    map (convertJobE jobs) <$> selectList [JobEStatus ==. PendingE] []
