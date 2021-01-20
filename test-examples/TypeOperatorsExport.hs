{-# OPTIONS_GHC -fplugin Plugin.SMLPlugin   #-}
{-# LANGUAGE NoImplicitPrelude              #-}
{-# LANGUAGE TypeOperators                  #-}
module TypeOperatorsExport (type (:+:)(..)) where

import Plugin.SMLPlugin.Prelude

data a :+: b = a :+: b
