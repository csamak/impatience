{-# LANGUAGE OverloadedStrings #-}
module Static.Entries
  ( jsEntries
  , settings
  )
where

import           WaiAppStatic.Storage.Embedded  ( EmbeddableEntry(..)
                                                , mkSettings
                                                )
import           Crypto.Hash                    ( Digest
                                                , MD5(..)
                                                , hashlazy
                                                )
import qualified Data.ByteString.Lazy          as BL
                                                ( readFile )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Language.Haskell.TH.Lib        ( ExpQ )
import           Language.Haskell.TH.Syntax     ( qAddDependentFile )
import           Network.Mime                   ( MimeType )

settings :: [(FilePath, IO EmbeddableEntry)] -> ExpQ
settings entries = do
  mapM_ (qAddDependentFile . fst) entries
  mkSettings (mapM snd entries) -- also consider servant-static-th as an alternative

entry :: String -> Text -> MimeType -> (String, IO EmbeddableEntry)
entry path location mime =
  ( path
  , do
    contents <- BL.readFile path
    return EmbeddableEntry
      { eLocation = location
      , eMimeType = mime
      , eContent  = Left (pack . show $ (hashlazy contents :: Digest MD5), contents)
      }
  )

-- TODO: don't hardcode this path. See https://github.com/tweag/rules_haskell/issues/1296
jsEntries :: [(FilePath, IO EmbeddableEntry)]
jsEntries =
  [ entry "bazel-out/k8-fastbuild/bin/site/impatience-bundle.js"
          "impatience.js"
          "application/javascript"
  ]
