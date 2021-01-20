{-# OPTIONS_GHC -fplugin Plugin.SMLPlugin   #-}
{-# LANGUAGE NoImplicitPrelude              #-}
module Data where

import Plugin.SMLPlugin.Prelude

-- Datatypes can be translated ...
data MyMaybe a = MyJust a
               | MyNothing

-- ... and newtypes as well.
newtype Wrap a = Proxy a

data Test a
