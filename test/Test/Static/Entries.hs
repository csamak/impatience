{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Test.Static.Entries where

import           Static.Entries
import           Codec.Compression.GZip
import           Data.ByteString.Lazy.Char8     ( unpack )
import           Language.JavaScript.Parser
import           Network.Wai.Application.Static
import           Network.Wai.Test
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import           Test.Tasty
import           Test.Tasty.Hedgehog

main = defaultMain $ testProperty "All served JS files are parseable" hprop_jsParseable

-- hardcoded to avoid depending on the filesystem
jsLocations = ["impatience.js"]
jsApp = staticApp $(settings jsEntries)

hprop_jsParseable = property $ do
  jsLocation <- forAll $ Gen.element jsLocations
  response   <- evalIO $ runSession (req jsLocation) jsApp
  _          <- evalEither $ parse (unpack . decompress $ simpleBody response) "name.js"
  length jsLocations === length jsEntries -- make sure the test isn't missing files
  where req e = request $ setPath defaultRequest e
