{-# OPTIONS_GHC -fplugin Plugin.SMLPlugin   #-}
{-# LANGUAGE NoImplicitPrelude              #-}
{-# LANGUAGE TypeOperators                  #-}
module TypeOperatorsImport where

import Plugin.SMLPlugin.Prelude
import TypeOperatorsExport (type (:+:)(..))

mapBoth :: (a -> c) -> (b -> d) -> a :+: b -> c :+: d
mapBoth f g (a :+: b) = f a :+: g b
