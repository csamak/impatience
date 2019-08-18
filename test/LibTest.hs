{-# LANGUAGE OverloadedStrings #-}
module LibTest where

import           Lib
import           Text.Blaze.Html.Renderer.String
import           Control.Monad
import           Data.Aeson
import           Data.List
import           Data.Proxy
import           Generic.Random
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import qualified Test.QuickCheck               as QC
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit               ( (@?) )
import           Servant
import           Servant.Swagger.Test

spec_json = context "ToJSON matches ToSchema" $ validateEveryToJSON (Proxy :: Proxy API)

evalHandler = evalIO . runHandler

genProgress :: Gen Progress
genProgress = Progress <$> integerGen <*> integerGen <*> integerGen
  where integerGen = Gen.int Range.linearBounded

hprop_unknownProgresIs404 = property $ do
  num    <- forAll $ Gen.int (Range.linear 3 2000)
  result <- evalHandler . progress $ num
  result === Left err404

hprop_knownProgressIsProgress = property $ do
  num    <- forAll $ Gen.int (Range.linear 1 2)
  result <- evalHandler . progress $ num
  void . evalEither $ result

hprop_progressToFromJsonIsIdentity = property $ do
  prog <- forAll genProgress
  Just prog === (decode . encode) prog

unit_annie = videoLink `isInfixOf` html @? html ++ " should remind us that others care about annie"
 where
  html      = renderHtml annie
  videoLink = "https://www.youtube.com/embed/h_D3VFfhvs4"

instance QC.Arbitrary Progress where
  arbitrary = genericArbitraryU
