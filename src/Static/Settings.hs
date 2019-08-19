{-# LANGUAGE TemplateHaskell #-}
-- |Settings for exe embedded static resources (e.g. js)
module Static.Settings
    ( jsSettings
    )
where

import           Static.Entries
import           WaiAppStatic.Types             ( StaticSettings )

jsSettings :: StaticSettings
jsSettings = $(settings jsEntries)
