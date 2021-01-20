{-# LANGUAGE NoImplicitPrelude              #-}
{-# LANGUAGE DefaultSignatures              #-}
{-# OPTIONS_GHC -fplugin Plugin.SMLPlugin   #-}
module DefaultSignaturesExport where

import Plugin.SMLPlugin.Prelude


class FromInt a where
  fromIntDefault :: Int -> a
  default fromIntDefault :: Integral a => Int -> a
  fromIntDefault x = fromInteger (toInteger x)

instance FromInt Int
