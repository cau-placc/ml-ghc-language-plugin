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
module Plugin.SMLPlugin.Encapsulation
  ( SML
  , runSML
  ) where

import Control.Monad.IO.Class
import Plugin.Effect.Monad

runSML :: (Normalform SML a b, MonadIO io) => SML a -> io b
runSML n = runEffect (nf n)
