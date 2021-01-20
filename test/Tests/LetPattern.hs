{-# OPTIONS_GHC -fplugin Plugin.SMLPlugin   #-}
{-# LANGUAGE NoImplicitPrelude              #-}
module Tests.LetPattern where

import Plugin.SMLPlugin.Prelude

letPattern :: Int
letPattern = let (Nothing, x) = (failed, 1) in x
