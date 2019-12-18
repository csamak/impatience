{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Database.Beam.Postgres
import Database.Beam.Query
import Database.Schema
import Database.Db
import Control.Monad.IO.Class

import           Data.Aeson
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger             ( withStdoutLogger )
import           Servant
import           Servant.HTML.Blaze             ( HTML )
import           Servant.Swagger
import           Static.Settings
import           Text.Blaze.Html                ( Html )
import           Text.Blaze.Html5               ( html
                                                , body
                                                , iframe
                                                , (!)
                                                , script
                                                )
import qualified Text.Blaze.Html5              as H
                                                ( head
                                                , title
                                                )
import           Text.Blaze.Html5.Attributes
import           Control.Lens
import           Data.Swagger                   ( Swagger
                                                , ToSchema
                                                , URL (..)
                                                , declareNamedSchema
                                                , info
                                                , license
                                                , url
                                                )

instance ToSchema Progress
instance ToJSON Progress
instance FromJSON Progress

type API = "annieareyouok" :> Get '[ HTML] Html
       :<|> "progress" :> Capture "id" Int :> Get '[ JSON] Progress
       :<|> "static" :> Raw
       :<|> "index.html" :> Get '[ HTML] Html
       :<|> Get '[ HTML] Html

type APIWithSwagger = "swagger.json" :> Get '[JSON] Swagger :<|> API

-- move to config file. allow multiple backends (sqlite) 
connectInfo :: ConnectInfo
connectInfo = ConnectInfo {
    connectHost = "db"
  , connectPort = 5432
  , connectUser = "postgres"
  , connectPassword = "localpass"
  , connectDatabase = "impatience"
  }

startApp :: IO ()
startApp = withStdoutLogger $ \logger -> do
  let settings = setPort 1234 $ setLogger logger defaultSettings
  conn <- connect connectInfo
  runSettings settings $ app conn

app :: Connection -> Application
app conn = serve api $ server conn

api :: Proxy APIWithSwagger
api = Proxy

server :: Connection -> Server APIWithSwagger
server conn =
  return swaggerDoc
    :<|> pure annie
    :<|> progress conn
    :<|> serveDirectoryWith jsSettings
    :<|> pure home
    :<|> pure home

annie =
  html
    $ iframe
    ! width "560"
    ! height "315"
    ! src "https://www.youtube.com/embed/h_D3VFfhvs4?autoplay=1&mute=1"
    $ mempty

home = html $ do
  H.head $ H.title "Sailor Greetings"
  body "Hello Sailor! (not dynamic)"
  script ! type_ "text/javascript" ! src "static/impatience.js" $ mempty

progress :: Connection -> Int -> Handler Progress
progress conn i = do
  found <- liftIO $ runBeamPostgresDebug putStrLn conn $
    runSelectReturningOne $ select $ filter_ (\p -> _progressId p ==. val_ i) (all_ (_progresses impatienceDb))
  maybe (throwError err404) pure found

instance ToSchema Html where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy String)
swaggerDoc =
  toSwagger (Proxy :: Proxy API)
    &  info
    .  license
    ?~ ("Apache" & url ?~ URL "https://www.apache.org/licenses/LICENSE-2.0")
