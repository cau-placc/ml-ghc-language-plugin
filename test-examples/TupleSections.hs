{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -fplugin Plugin.SMLPlugin   #-}
module TupleSections where

import Plugin.SMLPlugin.Prelude

test1 :: a -> (Int, a)
test1 = (0,)

test2 :: a -> (a, Int)
test2 = (,0)

test3 :: a -> b -> (a, b)
test3 = (,)

test4 :: a -> b -> (a, b)
test4 a b = (,b) a
