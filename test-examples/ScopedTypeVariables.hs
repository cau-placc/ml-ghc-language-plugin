{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin Plugin.SMLPlugin   #-}
module ScopedTypeVariables where

import Plugin.SMLPlugin.Prelude

appendAndReverseSelf :: forall a. [a] -> [a]
appendAndReverseSelf xs = ys ++ ys
     where
       ys :: [a]
       ys = reverse xs
