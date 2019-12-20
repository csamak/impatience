{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib where


import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Either.Combinators
import           Data.Int
import           Data.Text.Encoding             ( encodeUtf8 )
import           Database.Types
import           Database.Session
import           Dhall                          ( input, auto )
import qualified Hasql.Connection              as Connection
import           Hasql.Connection               ( Connection )
import qualified Hasql.Session                 as Session
                                                ( run )
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
                                                , URL(..)
                                                , declareNamedSchema
                                                , info
                                                , license
                                                , url
                                                )

instance ToSchema Progress
instance ToJSON Progress
instance FromJSON Progress

-- see https://github.com/lspitzner/brittany/issues/271
-- brittany-disable-next-binding
type API = "annieareyouok" :> Get '[ HTML] Html
       :<|> "progress" :> Capture "id" Int32 :> Get '[ JSON] Progress
       :<|> "static" :> Raw
       :<|> "index.html" :> Get '[ HTML] Html
       :<|> Get '[ HTML] Html

type APIWithSwagger = "swagger.json" :> Get '[JSON] Swagger :<|> API

startApp :: IO ()
startApp = withStdoutLogger $ \logger -> do
  let settings = setPort 1234 $ setLogger logger defaultSettings
  connString <- input auto "./impatience.dhall"
  -- use a connection pool
  connResult <- Connection.acquire $ encodeUtf8 connString
  case connResult of
    Left (Just errMsg) -> error $ show errMsg
    Left Nothing -> error "Unspecified connection error"
    Right conn -> runSettings settings $ app conn

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

progress :: Connection -> Int32 -> Handler Progress
progress conn i = do
  found <- liftIO $ Session.run (progressById i) conn
  -- handle query errors correctly
  maybe (throwError err404) pure $ fromRight Nothing found

instance ToSchema Html where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy String)
swaggerDoc =
  toSwagger (Proxy :: Proxy API)
    &  info
    .  license
    ?~ ("Apache" & url ?~ URL "https://www.apache.org/licenses/LICENSE-2.0")
