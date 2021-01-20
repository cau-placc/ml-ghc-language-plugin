{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fplugin Plugin.SMLPlugin   #-}
module GeneralizedNewtypeDeriving where

import Plugin.SMLPlugin.Prelude

newtype ListWrap a = L [a]
  deriving (Eq, Show, Functor, Applicative, Monad)
