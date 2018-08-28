{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
    describe "GET /users" $ do
        it "responds with 200" $ do
            get "/users" `shouldRespondWith` 200
        it "responds with [User]" $ do
            let users = "[{\"userId\":1,\"userFirstName\":\"Isaac\",\"userLastName\":\"Newton\"},{\"userId\":2,\"userFirstName\":\"Albert\",\"userLastName\":\"Einstein\"}]"
            get "/users" `shouldRespondWith` users

    describe "the route for checking if annie's state is passable" $ do
        it "confirms that she is indeed ok" $ do
            get "/annieareyouok" `shouldRespondWith` 200
        it "reminds us that others care about annie too" $ do
            get "/annieareyouok" `shouldRespondWith` "https://youtu.be/h_D3VFfhvs4"