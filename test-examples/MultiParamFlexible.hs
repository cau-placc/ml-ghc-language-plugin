{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fplugin Plugin.SMLPlugin   #-}
module MultiParamFlexible where

import Plugin.SMLPlugin.Prelude

class ListLike e l | l -> e where
  cons :: e -> l -> l
  uncons :: l -> Maybe (e, l)
  nil :: l

instance ListLike a [a] where
  cons = (:)
  uncons [] = Nothing
  uncons (x:xs) = Just (x, xs)
  nil = []

nilList :: ListLike e [e] => [e]
nilList = nil

-- Test undecidable instances
class M a b
class C a
instance M a a => C a
