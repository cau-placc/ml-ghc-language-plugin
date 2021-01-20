{-# OPTIONS_GHC -fplugin Plugin.SMLPlugin   #-}
{-# LANGUAGE NoImplicitPrelude              #-}
{-# LANGUAGE BangPatterns                   #-}
module Tests.Bang (testBang, testNoBang) where

import Plugin.SMLPlugin.Prelude

test :: a -> ()
test !_ = ()

testNo :: a -> ()
testNo a = ()

testBang :: ()
testBang = test (failed :: ())

testNoBang :: ()
testNoBang = testNo (failed :: ())
