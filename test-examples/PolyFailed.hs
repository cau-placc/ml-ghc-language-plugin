{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin Plugin.SMLPlugin   #-}
module PolyFailed where

import Plugin.SMLPlugin.Prelude

test :: String -> a
test _ = failed
