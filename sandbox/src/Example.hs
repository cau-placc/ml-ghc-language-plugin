{-# OPTIONS_GHC -fplugin Plugin.SMLPlugin #-}
module Example where

data Test = Test Int

testShare :: Test
testShare = Test (let _ = print "Hello" in 5 :: Int)
