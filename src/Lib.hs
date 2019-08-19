{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
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
import           Data.Map                       ( fromList
                                                , (!?)
                                                )
import           Data.Swagger                   ( Swagger
                                                , ToSchema
                                                , URL (..)
                                                , declareNamedSchema
                                                , info
                                                , license
                                                , url
                                                )
import           GHC.Generics                   ( Generic )
data Progress = Progress { jobId, completed, total :: Int } deriving (Eq, Show, Generic)

instance ToSchema Progress
$(deriveJSON defaultOptions ''Progress)

defaultProgressEntries = fromList $ map (\p -> (jobId p, p)) [Progress 1 5 50, Progress 2 3 10]

type API = "annieareyouok" :> Get '[ HTML] Html
       :<|> "progress" :> Capture "jobid" Int :> Get '[ JSON] Progress
       :<|> "static" :> Raw
       :<|> "index.html" :> Get '[ HTML] Html
       :<|> Get '[ HTML] Html

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
server =
  return swaggerDoc
    :<|> return annie
    :<|> progress
    :<|> serveDirectoryWith jsSettings
    :<|> return home
    :<|> return home

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

progress :: Int -> Handler Progress
progress p = maybe (throwError err404) return $ defaultProgressEntries !? p

instance ToSchema Html where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy String)
swaggerDoc =
  toSwagger (Proxy :: Proxy API)
    &  info
    .  license
    ?~ ("Apache" & url ?~ URL "https://www.apache.org/licenses/LICENSE-2.0")
