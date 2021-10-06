{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
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
  , bind, sequence, rtrn, rtrnFunc, fmp, shre, shreTopLevel, seqValue
  , rtrnFuncUnsafePoly, appUnsafePoly
  , annotateUnlifted, annotate, DynSL(..) )
  where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.IORef
import Data.Typeable
import Plugin.Effect.Annotation
import Plugin.Effect.Classes
import Plugin.Effect.Transformers
import Unsafe.Coerce
import Prelude hiding (sequence)

-- | The actual monad for nondeterminism used by the plugin.
newtype SML a = SML {unSML :: StrictT SML (DynFlowSabT IO) a}
  deriving (Functor, Applicative, Monad, Sharing)
    via StrictT SML (DynFlowSabT IO)
  deriving anyclass (SharingTop)

data DynSL = L | H deriving (Show, Eq, Ord, Bounded)

newtype DynFlowSabT m a = DynFlowSabT {
    runDynFlowSabT :: DynSL -> m (DynSL, a)
  } deriving (Functor)

instance Monad m => Applicative (DynFlowSabT m) where
  pure v = DynFlowSabT $ \pc -> pure (pc, v)

  x *> y = DynFlowSabT $ \pc ->
    runDynFlowSabT x pc
      *> runDynFlowSabT y pc

  df <*> da = DynFlowSabT $ \pc -> do
    (_, f) <- runDynFlowSabT df pc
    (s, a) <- runDynFlowSabT da pc
    return (s, f a)

instance (Monad m) => Monad (DynFlowSabT m) where
  (>>) = (*>)

  x >>= f = DynFlowSabT $ \pc -> do
    (l, v) <- runDynFlowSabT x pc
    let xo = f v
    runDynFlowSabT xo $ max pc l

-- should not use max but lub

instance MonadTrans DynFlowSabT where
  lift m = DynFlowSabT $ \pc -> do
    a <- m
    return (pc, a)

instance MonadFail m => MonadFail (DynFlowSabT m) where
  fail s = DynFlowSabT $ \_ -> fail s

liftAt :: (MonadFail m) => DynSL -> DynSL -> m a -> DynFlowSabT m a
liftAt li lo c =
  let levelError pc =
        "Output at level " ++ show li ++ " with "
          ++ "program counter at level "
          ++ show pc
          ++ "!"
   in DynFlowSabT $ \pc ->
        if pc <= li
          then do v <- c; return (lo, v)
          else fail $ levelError pc

annotateUnlifted :: DynSL -> DynSL -> SML (a --> a)
annotateUnlifted li lo = rtrnFunc $ \a ->
  annotate `app` (return li) `app` (return lo) `app` a

annotate :: SML (DynSL --> DynSL --> a --> a)
annotate = rtrnFunc $ \li ->
  li >>= \(li' :: DynSL) -> rtrnFunc $ \lo ->
    lo >>= \lo' ->
      rtrnFunc $ \(Strict s) -> Strict $
        DynFlowSabT $ \pc -> do
          runDynFlowSabT (liftAt li' lo' (snd <$> runDynFlowSabT s pc)) pc

--Strict $ TopSharingT $ DynFlowSabT $ \pc -> liftAt li' lo' (fa maxBound) -- TODO (fa maxBound) / (fa minBound) / (fa pc)?

{-# COMPLETE Strict #-}

pattern Strict :: DynFlowSabT IO a -> SML a
pattern Strict x = SML (StrictT x)

{-# INLINE[0] bind #-}
bind :: SML a -> (a -> SML b) -> SML b
bind = (>>=)

{-# INLINE[0] sequence #-}
sequence :: SML a -> SML b -> SML b
sequence = (>>)

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
"bind/rtrn" forall f x. bind (rtrn x) f = f x
"sequence/rtrn" forall x y. sequence (rtrn x) y = y
"shreTopLevel" forall x i. shreTopLevel i (rtrn x) = rtrn x
    #-}
  -- "bind/rtrn'let"   forall e x. let b = e in rtrn x = rtrn (let b = e in x)

runEffect :: MonadIO io => SML a -> io a
runEffect (SML a) = liftIO $ fmap snd $ runDynFlowSabT (runStrictT a) L

runEffectNF :: (Normalform SML a b, MonadIO io) => SML a -> io b
runEffectNF a = runEffect (nf a)

ref :: Shareable SML a => SML (a --> IORef a)
ref = rtrnFunc $ \(SML a) -> SML $
  a >>= \val -> lift $ liftAt L L $ newIORef val

readRef :: Shareable SML a => SML (IORef a --> a)
readRef = rtrnFunc $ \(SML ioref) -> SML $
  ioref >>= \r -> lift $ liftAt H H $ readIORef r

writeRef :: Shareable SML a => SML (IORef a --> a --> ())
writeRef = rtrnFunc $ \(SML ioref) -> rtrnFunc $ \(SML a) -> SML $
  ioref >>= \r -> a >>= \val -> lift $ liftAt H L $ writeIORef r val

data UserException = forall a. Typeable a => UEX a String DynSL

instance Show UserException where
  show (UEX _ disp _) = disp

instance Exception UserException

handleStrict :: forall a b. Typeable b => SML a -> SML (SML b -> SML a) -> SML a
handleStrict (Strict l1) (Strict l2) = Strict $
  DynFlowSabT $ \pc -> runDynFlowSabT l1 pc `catch` handleUE
  where
    handleUE :: UserException -> IO (DynSL, a)
    handleUE (UEX b d pc) = case cast b of
      Just b' ->
        runDynFlowSabT l2 pc >>= \(pc', f) ->
          runDynFlowSabT (runStrictT $ unSML $ f (return b')) pc'
      Nothing -> throw (UEX b d pc)

orElseStrict :: SML a -> SML a -> SML a
orElseStrict (Strict l1) (Strict l2) = Strict $
  DynFlowSabT $ \pc0 -> runDynFlowSabT l1 pc0 `catch` \(UEX _ _ pc2) ->
      runDynFlowSabT l2 pc2

throwStrict :: Typeable a => SML a -> String -> SML b
throwStrict (Strict l) disp = Strict $
  DynFlowSabT $ \pc -> do
    (pc', v) <- runDynFlowSabT l pc
    throw (UEX v disp pc')

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
