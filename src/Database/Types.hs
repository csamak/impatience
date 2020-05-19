{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Types where

import           Data.Int                       ( Int32 )
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           Data.Aeson.Types               ( FromJSON
                                                , ToJSON
                                                )
import           GHC.Generics                   ( Generic )

data Progress = Progress { _progress_id, _jobId, _completed, _total :: Int32, _reportedTime :: UTCTime }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
data Job = Job { _job_id :: Int32, _refId :: Text, _name :: Text, _startTime :: UTCTime }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
