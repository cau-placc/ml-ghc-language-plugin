{-# LANGUAGE TemplateHaskell #-}
module Main where

import Example
import Plugin.SMLPlugin.Encapsulation

main :: IO ()
main = do
  _ <- runSML testShare
  return ()
