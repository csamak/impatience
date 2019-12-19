{-
Required configuration for starting the impatience server.

The main exe will look for this file in the working directory.
-}

-- TODO: create a record with other settings.
-- For now, this is just a postgres connection string
(env:IMPATIENCE_CONN_STRING as Text) ? "postgresql://postgres:localpass@db/impatience"
