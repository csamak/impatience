module Database.Session where

import           Hasql.Session                  ( statement
                                                , Session
                                                )
import           Data.Int                       ( Int32 )
import           Database.Types
import qualified Database.Statement            as Statement

jobById :: Int32 -> Session (Maybe Job)
jobById i = statement i Statement.jobById

insertJob :: Job -> Session Int32
insertJob j = statement j Statement.insertJob

progressById :: Int32 -> Session (Maybe Progress)
progressById i = statement i Statement.progressById

insertProgress :: Progress -> Session Int32
insertProgress p = statement p Statement.insertProgress
