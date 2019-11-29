module Database.Db where

import           Database.Beam
import           Database.Beam.Migrate
import           Database.Beam.Migrate.Simple
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Migrate
import           Database.Schema

checkedSettings :: CheckedDatabaseSettings Postgres ImpatienceDb
checkedSettings = defaultMigratableDbSettings

-- lazy auto migrations. useful for development
migrateDB :: Connection -> IO ()
migrateDB conn = runBeamPostgresDebug putStrLn conn $ autoMigrate migrationBackend checkedSettings

impatienceDb :: DatabaseSettings Postgres ImpatienceDb
impatienceDb = unCheckDatabase checkedSettings
