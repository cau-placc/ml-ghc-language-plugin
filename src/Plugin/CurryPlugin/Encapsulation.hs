{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : Plugin.CurryPlugin.Encapsulation
Description : Encapsulation of Nondeterminism
Copyright   : (c) Kai-Oliver Prott (2020)
License     : BSD-3 Clause
Maintainer  : kai.prott@hotmail.de

This module contains functions to encapsulate the nondeterminism of
plugin-compiled functions.
-}
module Plugin.CurryPlugin.Encapsulation
  ( Nondet
  , runSML
  ) where

import Control.Monad.IO.Class
import Plugin.Effect.Monad

runSML :: (Normalform Nondet a b, MonadIO io) => Nondet a -> io b
runSML n = runEffect (nf n)
