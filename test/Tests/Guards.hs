{-# OPTIONS_GHC -fplugin Plugin.SMLPlugin   #-}
{-# LANGUAGE NoImplicitPrelude              #-}
module Tests.Guards where

import Plugin.SMLPlugin.Prelude

take :: Int -> [a] -> [a]
take n xs
  | 0 < n = unsafeTake n xs
  | otherwise = []

unsafeTake :: Int -> [a] -> [a]
unsafeTake _ [] = []
unsafeTake 1 (x : _) = [x]
unsafeTake m (x : xs) = x : unsafeTake (m - 1) xs
