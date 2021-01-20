{-# OPTIONS_GHC -fplugin Plugin.SMLPlugin   #-}
{-# LANGUAGE NoImplicitPrelude              #-}
module Tests.UnknownNat where

import Plugin.SMLPlugin.Prelude

unknownNat :: Int
unknownNat = 1 ? (unknownNat + 1)
