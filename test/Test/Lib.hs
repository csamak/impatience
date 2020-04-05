{-# LANGUAGE FlexibleInstances #-}

module Test.Lib where

import           Lib
import           Text.Blaze.Html.Renderer.String
import           Data.Aeson
import           Data.List
import           Data.Time
import           Database.Types
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Test.Tasty.HUnit               ( (@?) )

genProgress :: Gen Progress
genProgress = Progress <$> integerGen <*> integerGen <*> integerGen <*> integerGen <*> dateGen
  where
    integerGen = Gen.int32 Range.linearBounded
    dateGen = do
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

unit_annie = videoLink `isInfixOf` html @? html ++ " should remind us that others care about annie"
 where
  html      = renderHtml annie
  videoLink = "https://www.youtube.com/embed/h_D3VFfhvs4"
