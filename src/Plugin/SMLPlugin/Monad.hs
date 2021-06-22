{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-|
Module      : Plugin.SMLPlugin.Monad
Description : Convenience wrapper for the effect
Copyright   : (c) Kai-Oliver Prott (2020)
Maintainer  : kai.prott@hotmail.de

This module contains the actual monad used by the plugin and a few
convenicence functions.
The monad type is a wrapper over the
'Strict' type from 'Plugin.Effect.MLEffect'.
-}
module Plugin.SMLPlugin.Monad
  ( SML(..), type (-->)(..), share
  , Normalform(..), runEffect, runEffectNF
  , ref, readRef, writeRef, runIO
  , handleStrict, throwStrict, orElseStrict
  , SMLTag(..)
  , liftSML1, liftSML2
  , app, apply2, apply2Unlifted, apply3
  , bind, rtrn, rtrnFunc, fmp, shre, shreTopLevel, seqValue
  , rtrnFuncUnsafePoly, appUnsafePoly )
  where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Exception
import Data.IORef
import Data.Typeable
import Unsafe.Coerce

import Plugin.Effect.Classes
import Plugin.Effect.Transformers
import Plugin.Effect.Annotation

-- | The actual monad for nondeterminism used by the plugin.
newtype SML a = SML { unSML :: StrictT SML (TopSharingT SML IO) a }
  deriving (Functor, Applicative, Monad, Sharing)
                        via StrictT SML (TopSharingT SML IO)
  deriving (SharingTop) via TopSharingT SML IO

{-# COMPLETE Strict #-}
pattern Strict :: TopSharingT SML IO a -> SML a
pattern Strict x = SML (StrictT x)

{-# INLINE[0] bind #-}
bind :: SML a -> (a -> SML b) -> SML b
bind = (>>=)

{-# INLINE[0] rtrn #-}
rtrn :: a -> SML a
rtrn = pure

{-# INLINE[0] rtrnFunc #-}
rtrnFunc :: (SML a -> SML b) -> SML (a --> b)
rtrnFunc = pure . Func

{-# INLINE[0] app #-}
app :: SML (a --> b) -> SML a -> SML b
app mf ma = mf >>= \(Func f) -> f ma

-- HACK:
-- RankNTypes are not really supported for various reasons,
-- but to test rewrite rules, we needed them to be supported at least
-- when the functions with RankN types are used and defined in the same module.
-- However, imagine we have a lambda with a (rank 2) type
-- (forall x. blah) -> blub.
-- Its lifted variant is something like
-- (forall x. blah') --> blub'
-- If we "unpack" the (-->) type constructor we get
-- m (forall x. blah') -> m blub'
-- This is bad, because the lifted type of the argument (forall x. blah)
-- is (forall x. m blah') and not m (forall x. blah').
-- To remedy this, we provide the following two functions using unsafeCoerce to
-- accomodate such a RankN type.
{-# INLINE[0] rtrnFuncUnsafePoly #-}
rtrnFuncUnsafePoly :: forall a b a'. (a' -> SML b) -> SML (a --> b)
rtrnFuncUnsafePoly f = pure (Func (unsafeCoerce f :: SML a -> SML b))

{-# INLINE[0] appUnsafePoly #-}
appUnsafePoly :: forall a b a'. SML (a --> b) -> a' -> SML b
appUnsafePoly mf ma = mf >>= \(Func f) -> (unsafeCoerce f :: a' -> SML b) ma

{-# INLINE[0] fmp #-}
fmp :: (a -> b) -> SML a -> SML b
fmp = fmap

{-# INLINE[0] shre #-}
shre :: SML a -> SML (SML a)
shre = share

{-# INLINE seqValue #-}
seqValue :: SML a -> SML b -> SML b
seqValue a b = a >>= \a' -> a' `seq` b

{-# INLINE[0] shreTopLevel #-}
shreTopLevel :: (Int, String) -> SML a -> SML a
shreTopLevel = shareTopLevel

{-# RULES
"bind/rtrn"    forall f x. bind (rtrn x) f = f x
"shreTopLevel" forall x i. shreTopLevel i (rtrn x) = rtrn x
    #-}
  -- "bind/rtrn'let"   forall e x. let b = e in rtrn x = rtrn (let b = e in x)

runEffect :: MonadIO io => SML a -> io a
runEffect (SML a) = liftIO $ runTopSharingT (runStrictT a)

runEffectNF :: (Normalform SML a b, MonadIO io) => SML a -> io b
runEffectNF a = runEffect (nf a)

ref :: Shareable SML a => SML (a --> IORef a)
ref = rtrnFunc $ \(SML a) -> SML (
  a >>= \val -> lift $ lift $ newIORef val)

readRef :: Shareable SML a => SML (IORef a --> a)
readRef = rtrnFunc $ \(SML ioref) -> SML (
  ioref >>= \r -> lift $ lift $ readIORef r)

writeRef :: Shareable SML a => SML (IORef a --> a --> ())
writeRef = rtrnFunc $ \(SML ioref) -> rtrnFunc $ \(SML a) -> SML (
  ioref >>= \r -> a >>= \val -> lift $ lift $ writeIORef r val)

data UserException = forall a. Typeable a => UEX a String TopStore

instance Show UserException where
  show (UEX _ disp _) = disp

instance Exception UserException where

handleStrict :: forall a b. Typeable b
             => SML a -> SML (SML b -> SML a) -> SML a
handleStrict (Strict l1) (Strict l2) = Strict $ TopSharingT $ \c r -> do
  (a, r') <- runTopSharingTWith l1 r `catch` handleUE
  c a r'
  where
    handleUE :: UserException -> IO (a, TopStore)
    handleUE (UEX b d r) = case cast b of
      Just b' -> runTopSharingTWith l2 r >>= \(f, r') ->
                 runTopSharingTWith ( runStrictT $ unSML $ f (return b')) r'
      Nothing -> throw (UEX b d r)

orElseStrict :: SML a -> SML a -> SML a
orElseStrict (Strict l1) (Strict l2) = Strict $ TopSharingT $ \c r -> do
  (a, r') <- runTopSharingTWith l1 r `catch` \(UEX _ _ rx) ->
    runTopSharingTWith l2 rx
  c a r'

throwStrict :: Typeable a => SML a -> String -> SML b
throwStrict (Strict l) disp = Strict $ TopSharingT $ \_ r -> do
  (v, r') <- runTopSharingTWith l r
  throw (UEX v disp r')

runTopSharingTWith :: Monad m => TopSharingT n m a -> TopStore -> m (a, TopStore)
runTopSharingTWith m r = fromTopSharingT m (\a c -> return (a, c)) r

runIO :: IO a -> SML a
runIO io = SML (lift (lift io))

infixr 0 -->
newtype a --> b = Func (SML a -> SML b)

instance (Sharing m) => Shareable m (a --> b) where
  shareArgs (Func f) = fmap Func (shareArgs f)

instance (Normalform SML a1 a2, Normalform SML b1 b2)
  => Normalform SML (a1 --> b1) (a2 -> b2) where
    nf    mf =
      mf >> return (error "Plugin Error: Cannot capture function types")
    liftE mf = do
      f <- mf
      return (Func (liftE . fmap f . nf))

-- | Lift a unary function with the lifting scheme of the plugin.
liftSML1 :: (a -> b) -> SML (a --> b)
liftSML1 f = rtrnFunc (\a -> a >>= \a' -> return (f a'))

-- | Lift a 2-ary function with the lifting scheme of the plugin.
liftSML2 :: (a -> b -> c) -> SML (a --> b --> c)
liftSML2 f = rtrnFunc (\a  -> rtrnFunc (\b  ->
                a >>=  \a' -> b >>=     \b' -> return (f a' b')))

-- | Apply a lifted 2-ary function to its lifted arguments.
apply2 :: SML (a --> b --> c) -> SML a -> SML b -> SML c
apply2 f a b = app f a >>= \(Func f') -> f' b

-- | Apply a lifted 2-ary function to its arguments, where just the
-- first argument has to be lifted.
apply2Unlifted :: SML (a --> b --> c)
               -> SML a -> b -> SML c
apply2Unlifted f a b = app f a >>= \(Func f') -> f' (return b)

-- | Apply a lifted 3-ary function to its lifted arguments.
apply3 :: SML (a --> b --> c --> d)
       -> SML a -> SML b -> SML c -> SML d
apply3 f a b c = apply2 f a b >>= \(Func f') -> f' c
