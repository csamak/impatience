{-# LANGUAGE QuasiQuotes #-}

module Database.Statement where

import           Data.Coerce
import           Data.Int
import           Data.Tuple.Curry
import           Data.Profunctor
import           Hasql.Statement
import           Hasql.TH
import           Database.Types

progressById :: Statement Int32 (Maybe Progress)
progressById = dimap
  coerce
  (fmap (uncurryN Progress))
  [maybeStatement|
        select id :: int4, completed :: int4, total :: int4
        from "progresses"
        where id = $1 :: int4
        |]
