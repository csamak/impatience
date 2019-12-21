module Database.Session where

import           Hasql.Session                  ( statement
                                                , Session
                                                )
import           Data.Int                       ( Int32 )
import           Database.Types
import qualified Database.Statement            as Statement

progressById :: Int32 -> Session (Maybe Progress)
progressById i = statement i Statement.progressById
