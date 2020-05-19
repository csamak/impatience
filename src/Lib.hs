{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib where


import           Api.Job
import           Api.Progress
import           Api.Static
import           Api.Types
import           Control.Monad.Trans.Reader     ( runReaderT )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Database.Types
import           Dhall                          ( input
                                                , auto
                                                )
import           Hasql.Pool                     ( Pool
                                                , acquire
                                                )
import           Network.Wai.Handler.Warp       ( setLogger
                                                , setPort
                                                , runSettings
                                                , defaultSettings
                                                )
import           Network.Wai.Logger             ( withStdoutLogger )
import           Servant
import           Servant.Swagger                ( toSwagger )
import           Text.Blaze.Html                ( Html )
import           Control.Lens                   ( (&)
                                                , (?~)
                                                )
import           Data.Swagger                   ( Swagger
                                                , ToSchema
                                                , URL(..)
                                                , declareNamedSchema
                                                , info
                                                , license
                                                , url
                                                )

-- see https://github.com/lspitzner/brittany/issues/271
-- brittany-disable-next-binding
type API = "job" :> JobAPI :<|> "progress" :> ProgressAPI :<|> StaticAPI

type APIWithSwagger = "swagger.json" :> Get '[JSON] Swagger :<|> API

startApp :: IO ()
startApp = withStdoutLogger $ \logger -> do
  let settings = setPort 1234 $ setLogger logger defaultSettings
  connString <- input auto "./impatience.dhall"
  pool       <- acquire (10, 10, encodeUtf8 connString)
  runSettings settings $ app pool

app :: Pool -> Application
app pool = serve api $ hoistServer api (nt pool) server

nt :: Pool -> AppM a -> Handler a
nt p x = runReaderT (runAppM x) p

api :: Proxy APIWithSwagger
api = Proxy

server :: ServerT APIWithSwagger AppM
server = return swaggerDoc :<|> jobServer :<|> progressServer :<|> staticServer

instance ToSchema Job
instance ToSchema Progress
instance ToSchema Html where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy String)
swaggerDoc :: Swagger
swaggerDoc =
  toSwagger (Proxy :: Proxy API)
    &  info
    .  license
    ?~ ("Apache" & url ?~ URL "https://www.apache.org/licenses/LICENSE-2.0")
