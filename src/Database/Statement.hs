{-# LANGUAGE QuasiQuotes #-}

module Database.Statement where

import           Data.Coerce                    ( coerce )
import           Data.Int                       ( Int32 )
import           Data.Tuple.Curry               ( uncurryN )
import           Data.Profunctor                ( dimap )
import           Hasql.Statement                ( Statement )
import           Hasql.TH                       ( maybeStatement )
import           Database.Types

progressById :: Statement Int32 (Maybe Progress)
progressById =
  dimap coerce (fmap . uncurryN $ Progress)
  [maybeStatement|
        select id :: int4, completed :: int4, total :: int4
        from progresses
        where id = $1 :: int4
        |]
