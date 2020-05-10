{-# LANGUAGE QuasiQuotes #-}

module Database.Statement where

import           Data.Coerce                    ( coerce )
import           Data.Int                       ( Int32 )
import           Data.Time                      ( UTCTime )
import           Data.Tuple.Curry               ( uncurryN )
import           Data.Profunctor                ( dimap, lmap, rmap )
import           Data.Vector                    ( Vector, toList )
import           Hasql.Statement                ( Statement )
import           Hasql.TH                       ( maybeStatement, singletonStatement, vectorStatement )
import           Database.Types

jobById :: Statement Int32 (Maybe Job)
jobById =
  dimap coerce (fmap . uncurryN $ Job)
  [maybeStatement|
        select id :: int4, ref_id :: text, name :: text, start_time :: timestamptz
        from jobs
        where id = $1 :: int4
        |]

insertJob :: Statement Job Int32
insertJob =
  lmap (\(Job _ r n  _) -> (r, n))
  [singletonStatement|
        insert into jobs (ref_id, name)
        values ($1 :: text , $2 :: text)
        returning id :: int4
        |]

progressesByJob :: Statement Int32 [Progress]
progressesByJob =
  rmap (toList . fmap (uncurryN Progress))
  [vectorStatement|
        select id :: int4, job_id :: int4, completed :: int4, total :: int4, reported_time :: timestamptz
        from progresses
        where job_id = $1 :: int4
        |]

progressById :: Statement Int32 (Maybe Progress)
progressById =
  dimap coerce (fmap . uncurryN $ Progress)
  [maybeStatement|
        select id :: int4, job_id:: int4, completed :: int4, total :: int4, reported_time :: timestamptz
        from progresses
        where id = $1 :: int4
        |]

insertProgress :: Statement Progress Int32
insertProgress =
  lmap (\(Progress _ j c t _) -> (j, c, t))
  [singletonStatement|
        insert into progresses (job_id, completed, total)
        values ($1 :: int4, $2 :: int4, $3 :: int4)
        returning id :: int4
        |]
