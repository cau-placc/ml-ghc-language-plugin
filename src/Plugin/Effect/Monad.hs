{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE DeriveLift                 #-}
{-|
Module      : Plugin.Effect.Monad
Description : Convenience wrapper for the effect
Copyright   : (c) Kai-Oliver Prott (2020)
Maintainer  : kai.prott@hotmail.de

This module contains the actual monad used by the plugin and a few
convenicence functions.
The monad type is a wrapper over the
'Lazy' type from 'Plugin.Effect.CurryEffect'.
-}
module Plugin.Effect.Monad
  ( Nondet(..), type (-->), share
  , Normalform(..), runEffect
  , globalRef, ref, readRef, runIO
  , NondetTag(..)
  , liftNondet1, liftNondet2
  , apply1, apply2, apply2Unlifted, apply3
  , bind, rtrn, fmp, shre, seqValue)
  where

import Control.Monad.IO.Class
import Data.IORef

import Plugin.Effect.CurryEffect
import Plugin.Effect.Classes     (Sharing(..), Shareable(..), Normalform(..))
import Plugin.Effect.Annotation

-- | The actual monad for nondeterminism used by the plugin.
data Nondet a = Nondet { unNondet :: Lazy a }
  deriving Functor

{-# INLINE[0] bind #-}
bind :: Nondet a -> (a -> Nondet b) -> Nondet b
bind (Nondet a) f = Nondet (a >>= unNondet . f)

{-# INLINE[0] rtrn #-}
rtrn :: a -> Nondet a
rtrn a = Nondet (pureL a)

{-# INLINE[0] fmp #-}
fmp :: (a -> b) -> Nondet a -> Nondet b
fmp f (Nondet a) = Nondet (fmap f a)

{-# INLINE[0] shre #-}
shre :: Shareable Nondet a => Nondet a -> Nondet (Nondet a)
shre m = Nondet $ fmap Nondet $ memo (unNondet (m >>= shareArgs share))

{-# INLINE seqValue #-}
seqValue :: Nondet a -> Nondet b -> Nondet b
seqValue a b = a >>= \a' -> a' `seq` b

{-# RULES
"bind/rtrn"       forall f x. bind (rtrn x) f = f x
  #-}
  -- "bind/rtrn'let"   forall e x. let b = e in rtrn x = rtrn (let b = e in x)

instance Applicative Nondet where
  pure = rtrn
  Nondet f <*> Nondet a = Nondet (f <*> a)

instance Monad Nondet where
  (>>=) = bind

instance MonadFail Nondet where
  fail s = Nondet (fail s)

instance Sharing Nondet where
  share = shre

runEffect :: MonadIO io => Nondet a -> io a
runEffect (Nondet a) = runLazy a

globalRef :: Nondet (Int --> a --> IORef a)
globalRef = return $ \(Nondet v) -> return $ \(Nondet a) -> Nondet (
  v >>= \i -> a >>= \val -> getOrCreateGlobalRefWith i val)

ref :: Nondet (a --> IORef a)
ref = return $ \(Nondet a) -> Nondet (
  a >>= \val -> liftIOInLazy $ newIORef val)

readRef :: Nondet (IORef a --> a)
readRef = return $ \(Nondet ioref) -> Nondet (
  ioref >>= \r -> liftIOInLazy $ readIORef r)

runIO :: IO a -> Nondet a
runIO io = Nondet (liftIOInLazy io)

infixr 0 -->
type a --> b = (Nondet a -> Nondet b)

-- | Lift a unary function with the lifting scheme of the plugin.
liftNondet1 :: (a -> b) -> Nondet (a --> b)
liftNondet1 f = return (\a -> a >>= \a' -> return (f a'))

-- | Lift a 2-ary function with the lifting scheme of the plugin.
liftNondet2 :: (a -> b -> c) -> Nondet (a --> b --> c)
liftNondet2 f = return (\a  -> return (\b  ->
                a >>=   \a' -> b >>=   \b' -> return (f a' b')))

-- | Apply a lifted unary function to its lifted argument.
apply1 :: Nondet (a --> b) -> Nondet a -> Nondet b
apply1 f a = f >>= ($ a)

-- | Apply a lifted 2-ary function to its lifted arguments.
apply2 :: Nondet (a --> b --> c) -> Nondet a -> Nondet b -> Nondet c
apply2 f a b = apply1 f a >>= ($ b)

-- | Apply a lifted 2-ary function to its arguments, where just the
-- first argument has to be lifted.
apply2Unlifted :: Nondet (a --> b --> c)
               -> Nondet a -> b -> Nondet c
apply2Unlifted f a b = apply1 f a >>= ($ return b)

-- | Apply a lifted 3-ary function to its lifted arguments.
apply3 :: Nondet (a --> b --> c --> d)
       -> Nondet a -> Nondet b -> Nondet c -> Nondet d
apply3 f a b c = apply2 f a b >>= ($ c)
