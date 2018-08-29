{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp
  , app
  )
where

import           Data.Aeson
import           Data.Aeson.TH
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger             ( withStdoutLogger )
import           Servant
import           Data.List
import qualified Data.Map                      as M

data Progress = Progress
  { jobId :: Integer
  , completed :: Int
  , total :: Int
  }

$(deriveJSON defaultOptions ''Progress)

defaultProgressEntries = M.fromList $ map (\p -> (jobId p, p)) [Progress 1 5 50, Progress 2 3 10]

type API
   = "annieareyouok" :> Get '[ PlainText] String
    :<|> "progress" :> Capture "jobid" Integer :> Get '[ JSON] Progress

startApp :: IO ()
startApp = withStdoutLogger $ \logger -> do
  let settings = setPort 1234 $ setLogger logger defaultSettings
  runSettings settings app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return annie :<|> progress

annie = "https://youtu.be/h_D3VFfhvs4"

progress :: Integer -> Handler Progress
progress id = maybe (throwError err404) return $ find (\p -> jobId p == id) defaultProgressEntries
