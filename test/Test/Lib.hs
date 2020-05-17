{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.Lib where

import           Data.Aeson
import           Data.Time
import           Database.Types
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Test.Tasty
import           Test.Tasty.Hedgehog

main = defaultMain $ testGroup
  "Tests"
  [testProperty "Progress JSON round trips" hprop_progressToFromJsonIsIdentity]

genProgress :: Gen Progress
genProgress = Progress <$> integerGen <*> integerGen <*> integerGen <*> integerGen <*> dateGen
 where
  integerGen = Gen.int32 Range.linearBounded
  dateGen    = do
    y <- toInteger <$> Gen.int (Range.constant 2000 2019)
    m <- Gen.int (Range.constant 1 12)
    d <- Gen.int (Range.constant 1 28)
    let day = fromGregorian y m d
    secs <- toInteger <$> Gen.int (Range.constant 0 86400)
    let diffTime = secondsToDiffTime secs
    pure $ UTCTime day diffTime

hprop_progressToFromJsonIsIdentity = property $ do
  prog <- forAll genProgress
  tripping prog encode decode
