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
import           Data.Text                      ( pack )
import           Language.Haskell.TH.Lib        ( ExpQ )
import           Language.Haskell.TH.Syntax     ( qAddDependentFile )

settings :: [(FilePath, IO EmbeddableEntry)] -> ExpQ
settings entries = do
    mapM_ (qAddDependentFile . fst) entries
<<<<<<< HEAD
    mkSettings (mapM snd entries) -- also consider servant-static-th as an alternative
=======
    mkSettings (mapM snd entries)
>>>>>>> origin/master

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

jsEntries :: [(FilePath, IO EmbeddableEntry)]
jsEntries = [entry "static/app.js" "impatience.js" "application/javascript"]
