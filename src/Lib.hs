{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger       (withStdoutLogger)
import Servant

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "annieareyouok" :> Get '[JSON] String
    :<|> "users" :> Get '[JSON] [User]

startApp :: IO ()
startApp = do
    withStdoutLogger $ \logger -> do
        let settings = setPort 1234 $ setLogger logger defaultSettings
        runSettings settings app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return annie :<|> return users

annie = "https://youtu.be/h_D3VFfhvs4"

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
