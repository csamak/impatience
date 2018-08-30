{-# LANGUAGE OverloadedStrings #-}
module LibSpec
  ( spec
  )
where

import           Lib
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Data.Proxy
import           Generic.Random
import           Test.QuickCheck
import           Servant.Swagger.Test

spec :: Spec
spec = do

  describe "Swagger" $
   context "ToJSON matches ToSchema" $ validateEveryToJSON (Proxy :: Proxy API)

  with (return app) $ do
    describe "GET /progress/id" $ do
      it "responds with 200" $ get "/progress/1" `shouldRespondWith` 200
      it "responds with Progress" $ do
        get "/progress/1" `shouldRespondWith` "{\"jobId\":1,\"completed\":5,\"total\":50}"
        get "/progress/2" `shouldRespondWith` "{\"jobId\":2,\"completed\":3,\"total\":10}"
      it "responds with 404 when the job does not exist" $ get "/progress/3" `shouldRespondWith` 404

    describe "the route for checking if annie's state is passable" $ do
      it "confirms that she is indeed ok" $ get "/annieareyouok" `shouldRespondWith` 200
      it "reminds us that others care about annie too" $
        get "/annieareyouok" `shouldRespondWith` "https://youtu.be/h_D3VFfhvs4"

instance Arbitrary Progress where
  arbitrary = genericArbitraryU
