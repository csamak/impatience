{-# LANGUAGE QuasiQuotes #-}

module Database.Statement where

import           Data.Coerce                    ( coerce )
import           Data.Int                       ( Int32 )
import           Data.Tuple.Curry               ( uncurryN )
import           Data.Profunctor                ( dimap, lmap )
import           Hasql.Statement                ( Statement )
import           Hasql.TH                       ( maybeStatement, singletonStatement )
import           Database.Types

progressById :: Statement Int32 (Maybe Progress)
progressById =
  dimap coerce (fmap . uncurryN $ Progress)
  [maybeStatement|
        select id :: int4, completed :: int4, total :: int4
        from progresses
        where id = $1 :: int4
        |]

insertProgress :: Statement Progress Int32
insertProgress =
  lmap (\(Progress _ c t) -> (c, t))
  [singletonStatement|
        insert into progresses (completed, total)
        values ($1 :: int4, $2 :: int4)
        returning id :: int4
        |]