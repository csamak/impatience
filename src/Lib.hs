{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import           Data.Aeson
import           Data.Aeson.TH
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger             ( withStdoutLogger )
import           Servant
import           Servant.Swagger
import           Control.Lens
import           Data.Map                       ( fromList
                                                , (!?)
                                                )
import           Data.Swagger
import           GHC.Generics                   ( Generic )

data Progress = Progress { jobId, completed, total :: Int } deriving (Eq, Show, Generic)

instance ToSchema Progress
$(deriveJSON defaultOptions ''Progress)

defaultProgressEntries = fromList $ map (\p -> (jobId p, p)) [Progress 1 5 50, Progress 2 3 10]

type API = "annieareyouok" :> Get '[ PlainText] String :<|> "progress" :> Capture "jobid" Int :> Get '[ JSON] Progress

type APIWithSwagger = "swagger.json" :> Get '[JSON] Swagger :<|> API

startApp :: IO ()
startApp = withStdoutLogger $ \logger -> do
  let settings = setPort 1234 $ setLogger logger defaultSettings
  runSettings settings app

app :: Application
app = serve api server

api :: Proxy APIWithSwagger
api = Proxy

server :: Server APIWithSwagger
server = return swaggerDoc :<|> return annie :<|> progress

annie = "https://youtu.be/h_D3VFfhvs4"

progress :: Int -> Handler Progress
progress p = maybe (throwError err404) return $ defaultProgressEntries !? p

swaggerDoc =
  toSwagger (Proxy :: Proxy API)
    &  info
    .  license
    ?~ ("Apache" & url ?~ URL "https://www.apache.org/licenses/LICENSE-2.0")
