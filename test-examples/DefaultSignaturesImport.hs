{-# LANGUAGE NoImplicitPrelude              #-}
{-# LANGUAGE DefaultSignatures              #-}
{-# OPTIONS_GHC -fplugin Plugin.SMLPlugin   #-}
module DefaultSignaturesImport where

import Plugin.SMLPlugin.Prelude

import DefaultSignaturesExport

instance FromInt Integer

test1 :: Int
test1 = fromIntDefault 0

test2 :: Integer
test2 = fromIntDefault 0
