{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Api.Static
    ( StaticAPI
    , staticServer
    )
where

import           Servant
import           Servant.HTML.Blaze             ( HTML )
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
import           Text.Blaze.Html5.Attributes    ( height
                                                , src
                                                , type_
                                                , width
                                                )

-- brittany-disable-next-binding
type StaticAPI =  "annieareyouok" :> Get '[ HTML] Html
                  :<|> "static" :> Raw
                  :<|> "index.html" :> Get '[ HTML] Html
                  :<|> Get '[ HTML] Html

staticServer :: Applicative m => ServerT StaticAPI m
staticServer = pure annie :<|> serveDirectoryWith jsSettings :<|> pure home :<|> pure home

annie :: Html
annie =
    html
        $ iframe
        ! width "560"
        ! height "315"
        ! src "https://www.youtube.com/embed/h_D3VFfhvs4?autoplay=1&mute=1"
        $ mempty

home :: Html
home = html $ do
    H.head $ H.title "Sailor Greetings"
    body "Hello Sailor! (not dynamic)"
    script ! type_ "text/javascript" ! src "static/impatience.js" $ mempty

