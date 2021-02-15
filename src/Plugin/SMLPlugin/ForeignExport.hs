{-|
Module      : Plugin.CurryPlugin.ForeignExport
Description : Collection of foreign definitions for the Prelude
Copyright   : (c) Kai-Oliver Prott (2020)
License     : BSD-3 Clause
Maintainer  : kai.prott@hotmail.de

This module re-exports definitions that are required for the Prelude of
our Plugin. Note that we re-export Haskell's original defintions from the
Prelude of Base packages, while the built-In module is just added so that it
is loaded in every plugin-compiled module.
-}
module Plugin.SMLPlugin.ForeignExport
  ( Bool(..), Int, Integer, Char, String, Ordering(..)
  , Ratio, Rational, Float, Double
  , Show(..), Eq(..), Ord(..)
  , Num(..), Fractional(..), Real(..), Integral(..), Enum(..), Bounded(..)
  , Functor(..), Applicative(..), Alternative(..), Monad(..), MonadFail(..)
  , IsString(..)
  , B.putStr, B.putStrLn, B.print, B.getChar, B.getLine
  , B.handle, B.raise, B.orElse
  , ref, readRef, writeRef
  , IORef
  ) where

import Data.Ratio
import Data.String
import Data.IORef

import Control.Applicative

import Plugin.SMLPlugin.Monad
import Plugin.SMLPlugin.BuiltIn as B
  ( putStr, putStrLn, print, getChar, getLine
  , handle, raise, orElse )

{-# ANN module StandardML #-}
