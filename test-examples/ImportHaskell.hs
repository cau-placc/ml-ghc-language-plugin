{-# OPTIONS_GHC -fplugin Plugin.SMLPlugin   #-}
module ImportHaskell where

import Plugin.SMLPlugin.Prelude
import ExportHaskell

-- Compilation of this module is expected to fail
-- due to import of a plain Haskell module (see ExportHaskell.hs).
