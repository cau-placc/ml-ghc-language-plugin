{-# LANGUAGE KindSignatures                 #-}
{-# LANGUAGE StandaloneKindSignatures       #-}
{-# LANGUAGE StarIsType                     #-}
{-# OPTIONS_GHC -fplugin Plugin.SMLPlugin   #-}
module KindSignatures where

import Plugin.SMLPlugin.Prelude

type X :: *
data X = X

type MB :: * -> *
data MB (a :: *) = J a | N

-- Constraint is not in scope, it is available in Data.Kind
-- type Mnd :: (* -> *) -> Constraint
class Mnd (m :: (* -> *)) where
  rtrn :: a -> m a
