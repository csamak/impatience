{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib where


import           Api.Job
import           Api.Progress
import           Api.Static
import           Data.Int                       ( Int32 )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Database.Types
import           Dhall                          ( input
                                                , auto
                                                )
import qualified Hasql.Connection              as Connection
import           Hasql.Connection               ( Connection )
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
  -- use a connection pool
  connResult <- Connection.acquire $ encodeUtf8 connString
  case connResult of
    Left  (Just errMsg) -> error $ show errMsg
    Left  Nothing       -> error "Unspecified connection error"
    Right conn          -> runSettings settings $ app conn

app :: Connection -> Application
app conn = serve api $ server conn

api :: Proxy APIWithSwagger
api = Proxy

server :: Connection -> Server APIWithSwagger
server conn = return swaggerDoc :<|> jobServer conn :<|> progressServer conn :<|> staticServer

instance ToSchema Job
instance ToSchema Progress
instance ToSchema Html where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy String)
swaggerDoc =
  toSwagger (Proxy :: Proxy API)
    &  info
    .  license
    ?~ ("Apache" & url ?~ URL "https://www.apache.org/licenses/LICENSE-2.0")
