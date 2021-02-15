{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : Plugin.CurryPlugin.Encapsulation
Description : Encapsulation of Nondeterminism
Copyright   : (c) Kai-Oliver Prott (2020)
License     : BSD-3 Clause
Maintainer  : kai.prott@hotmail.de

This module contains functions to encapsulate the IO effects of
plugin-compiled functions.
-}
module Plugin.SMLPlugin.Encapsulation
  ( SML
  , runSML, runSML1, runSML2
  , runGeneric, runN
  ) where

import Control.Monad.IO.Class
import Plugin.SMLPlugin.Monad
import Plugin.SMLPlugin.THEval

-- | Run a nullary ML function.
runSML :: (Normalform SML a b, MonadIO io) => SML a -> io b
runSML = $(runN 0)

-- | Run a unary ML function.
runSML1 :: (Normalform SML a1 a2, Normalform SML b1 b2, MonadIO io)
        => SML (a1 --> b1) -> a2 -> io b2
runSML1 = $(runN 1)

-- | Run a 2-ary ML function.
runSML2 :: ( Normalform SML a1 a2
           , Normalform SML b1 b2
           , Normalform SML c1 c2
           , MonadIO io)
        => SML (a1 --> b1 --> c1) -> a2 -> b2 -> io c2
runSML2 = $(runN 2)
