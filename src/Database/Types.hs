{-# LANGUAGE DeriveGeneric #-}

module Database.Types where

import           Data.Int
import           GHC.Generics                   ( Generic )

data Progress = Progress { _jobId, _completed, _total :: Int32 } deriving (Eq, Show, Generic)