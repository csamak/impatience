module Database.Session where

import           Hasql.Session
import           Data.Int
import           Database.Types
import qualified Database.Statement            as Statement

progressById :: Int32 -> Session (Maybe Progress)
progressById i = statement i Statement.progressById
