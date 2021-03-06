{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fplugin Plugin.SMLPlugin   #-}
module OverloadedStrings where

import Plugin.SMLPlugin.Prelude

newtype StringWrap = S String
  deriving Eq

instance IsString StringWrap where
  fromString = S

test :: IsString a => a
test = "Hello"

testX :: Int
testX = case (test :: StringWrap) of
  "Hello" -> 1
  _       -> 0
