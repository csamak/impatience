{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Schema where

import           Database.Beam

data ProgressT f
    = Progress
    { _progressId :: C f Int -- make autoincrement
    , _completed :: C f Int
    , _total :: C f Int }
    deriving (Generic, Beamable)

type Progress = ProgressT Identity
deriving instance Show Progress
deriving instance Eq Progress
deriving instance Show (PrimaryKey ProgressT Identity)

instance Table ProgressT where
  data PrimaryKey ProgressT f = ProgressId (C f Int) deriving (Generic, Beamable)
  primaryKey = ProgressId . _progressId
type ProgressId = PrimaryKey ProgressT Identity

data ImpatienceDb f
    = ImpatienceDb { _progresses :: f (TableEntity ProgressT) } deriving (Generic, Database be)
