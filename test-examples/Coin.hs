{-# OPTIONS_GHC -fplugin Plugin.SMLPlugin   #-}
{-# LANGUAGE NoImplicitPrelude              #-}
module Coin where

import Plugin.SMLPlugin.Prelude

-- Nondeterministic choice between True and False.
coin :: Bool
coin = True ? False
