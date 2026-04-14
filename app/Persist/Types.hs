{-# LANGUAGE TemplateHaskell #-}

module Persist.Types
  ( JobStatusE(..)
  ) where

import Database.Persist.TH (derivePersistField)

data JobStatusE = PendingE | DoneE | FailedE
  deriving (Show, Read, Eq)
derivePersistField "JobStatusE"
