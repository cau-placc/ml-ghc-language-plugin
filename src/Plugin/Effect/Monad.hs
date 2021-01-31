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
'Strict' type from 'Plugin.Effect.MLEffect'.
-}
module Plugin.Effect.Monad
  ( SML(..), type (-->), share
  , Normalform(..), runEffect
  , ref, readRef, writeRef, runIO
  , handleStrict, throwStrict, orElseStrict
  , SMLTag(..)
  , liftSML1, liftSML2
  , apply1, apply2, apply2Unlifted, apply3
  , bind, rtrn, fmp, shre, shreTopLevel, seqValue)
  where

import Control.Monad.IO.Class
import Data.IORef

import Plugin.Effect.MLEffect
import Plugin.Effect.Classes     (Sharing(..), Shareable(..), Normalform(..))
import Plugin.Effect.Annotation

-- | The actual monad for nondeterminism used by the plugin.
data SML a = SML { unSML :: Strict a }
  deriving Functor

{-# INLINE[0] bind #-}
bind :: SML a -> (a -> SML b) -> SML b
bind (SML a) f = SML (a >>= unSML . f)

{-# INLINE[0] rtrn #-}
rtrn :: a -> SML a
rtrn a = SML (pureS a)

{-# INLINE[0] fmp #-}
fmp :: (a -> b) -> SML a -> SML b
fmp f (SML a) = SML (fmap f a)

{-# INLINE[0] shre #-}
shre :: SML a -> SML (SML a)
shre m = m >>= return . return

{-# INLINE seqValue #-}
seqValue :: SML a -> SML b -> SML b
seqValue a b = a >>= \a' -> a' `seq` b

{-# INLINE[0] shreTopLevel #-}
shreTopLevel :: (Int, String) -> SML a -> SML a
shreTopLevel key (SML act) = SML $ readOrExecuteTop key act

{-# RULES
"bind/rtrn"    forall f x. bind (rtrn x) f = f x
"shreTopLevel" forall x i. shreTopLevel i (rtrn x) = rtrn x
    #-}
  -- "bind/rtrn'let"   forall e x. let b = e in rtrn x = rtrn (let b = e in x)

instance Applicative SML where
  pure = rtrn
  SML f <*> SML a = SML (f <*> a)

instance Monad SML where
  (>>=) = bind

instance MonadFail SML where
  fail s = SML (fail s)

instance Sharing SML where
  share = shre
  shareTopLevel = shreTopLevel

runEffect :: MonadIO io => SML a -> io a
runEffect (SML a) = runStrict a

ref :: Shareable SML a => SML (a --> IORef a)
ref = return $ \(SML a) -> SML (
  a >>= \val -> liftIOInStrict $ newIORef val)

readRef :: Shareable SML a => SML (IORef a --> a)
readRef = return $ \(SML ioref) -> SML (
  ioref >>= \r -> liftIOInStrict $ readIORef r)

writeRef :: Shareable SML a => SML (SML (IORef a) -> SML (SML a -> SML ()))
writeRef = return $ \(SML ioref) -> return $ \(SML a) -> SML (
  ioref >>= \r -> a >>= \val -> liftIOInStrict $ writeIORef r val)

runIO :: IO a -> SML a
runIO io = SML (liftIOInStrict io)

infixr 0 -->
type a --> b = (SML a -> SML b)

-- | Lift a unary function with the lifting scheme of the plugin.
liftSML1 :: (a -> b) -> SML (a --> b)
liftSML1 f = return (\a -> a >>= \a' -> return (f a'))

-- | Lift a 2-ary function with the lifting scheme of the plugin.
liftSML2 :: (a -> b -> c) -> SML (a --> b --> c)
liftSML2 f = return (\a  -> return (\b  ->
                a >>=   \a' -> b >>=   \b' -> return (f a' b')))

-- | Apply a lifted unary function to its lifted argument.
apply1 :: SML (a --> b) -> SML a -> SML b
apply1 f a = f >>= ($ a)

-- | Apply a lifted 2-ary function to its lifted arguments.
apply2 :: SML (a --> b --> c) -> SML a -> SML b -> SML c
apply2 f a b = apply1 f a >>= ($ b)

-- | Apply a lifted 2-ary function to its arguments, where just the
-- first argument has to be lifted.
apply2Unlifted :: SML (a --> b --> c)
               -> SML a -> b -> SML c
apply2Unlifted f a b = apply1 f a >>= ($ return b)

-- | Apply a lifted 3-ary function to its lifted arguments.
apply3 :: SML (a --> b --> c --> d)
       -> SML a -> SML b -> SML c -> SML d
apply3 f a b c = apply2 f a b >>= ($ c)
