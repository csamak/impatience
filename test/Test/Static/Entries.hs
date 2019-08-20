{-# LANGUAGE TemplateHaskell #-}
module Test.Static.Entries where

import           Static.Entries
import           Codec.Compression.GZip
import           Control.Monad.IO.Class
import           Data.ByteString.Lazy.Char8
import           Data.Text.Encoding
import           Language.JavaScript.Parser
import           WaiAppStatic.Storage.Embedded
import           Network.Wai.Application.Static
import           Network.Wai.Test
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen

jsApp = staticApp $(settings jsEntries)

hprop_jsParseable = property $ do
  jsEntry  <- forAllWith (show . fst) $ Gen.element jsEntries
  entry    <- liftIO . snd $ jsEntry
  response <- liftIO $ runSession (req (encodeUtf8 . eLocation $ entry)) jsApp
  _        <- evalEither $ parse (unpack . decompress $ simpleBody response) "name.js"
  success
  where req e = request $ setPath defaultRequest e
