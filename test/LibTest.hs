{-# LANGUAGE OverloadedStrings #-}
module LibTest where

import           Lib
import           Control.Monad.Trans.Class
import           Data.Either
import           Data.Proxy
import           Generic.Random
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import qualified Test.QuickCheck               as QC
import           Test.Tasty.Hedgehog
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit               ( (@=?) )
import           Servant
import           Servant.Swagger.Test

spec_json = context "ToJSON matches ToSchema" $ validateEveryToJSON (Proxy :: Proxy API)

hprop_unknownProgresIs404 = property $ do
  num    <- forAll $ Gen.int (Range.linear 3 2000)
  result <- lift . runHandler $ progress (toInteger num)
  result === Left err404

hprop_knownProgressIsProgress = property $ do
  num    <- forAll $ Gen.int (Range.linear 1 2)
  result <- lift . runHandler $ progress (toInteger num)
  assert $ isRight result

unit_annie = annie @=? "https://youtu.be/h_D3VFfhvs4"

instance QC.Arbitrary Progress where
  arbitrary = genericArbitraryU
