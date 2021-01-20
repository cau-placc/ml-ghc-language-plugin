{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-|
Module      : Plugin.Effect.CurryEffect
Description : Implementation of nondeterminism with sharing
Copyright   : (c) Kai-Oliver Prott (2020)
Maintainer  : kai.prott@hotmail.de

This module contains defintions for a nondeterminism monad with support for
explicit sharing of computations.
It is adapted from a library explicit-sharing and a paper
by Sebastian Fischer et al.
"Purely functional lazy nondeterministic programming",
with slight modifications in the definition of the 'Lazy' datatype
and the 'collect' function.
We also had to adapt the implementation to the new GHC library, by mainly
adding a few missing instances.
-}
module Plugin.Effect.CurryEffect where

import qualified Data.IntMap         as M
import           Data.IORef
import           Data.Typeable
import           Control.Exception
import           Control.Monad
import           Control.Monad.State ( MonadState(..), MonadIO(..), gets, modify )
import           Unsafe.Coerce

import Plugin.Effect.Classes

-- | A Lazy implementation of a monad for nondeterminism
-- with support for explicit sharing.
newtype Lazy a = Lazy {
    fromLazy :: forall n. (a -> Store -> RefStore -> IO n)
             -> Store -> RefStore -> IO n
  } deriving Functor

-- | Collect all results into a given nondeterministic data structure.
runLazy :: MonadIO io => Lazy n -> io n
runLazy m = liftIO $ fromLazy m (\a _ _ -> return a) emptyStore emptyRefStore

liftIOInLazy :: IO a -> Lazy a
liftIOInLazy io = Lazy (\c s r -> io >>= \v -> c v s r)

runLazyWith :: MonadIO io
            => Lazy n -> Store -> RefStore -> io (n, Store, RefStore)
runLazyWith m s r = liftIO $ fromLazy m (\a b c -> return (a, b, c)) s r

data UserException = forall a. Typeable a => UEX a String Store RefStore

instance Show UserException where
  show (UEX _ disp _ _) = disp

instance Exception UserException where

handleLazy :: forall a b. Typeable b
           => Lazy a -> Lazy (Lazy b -> Lazy a) -> Lazy a
handleLazy l1 l2 = Lazy $ \c s r -> do
  (a, s', r') <- runLazyWith l1 s r `catch` handleUE
  c a s' r'
  where
    handleUE :: UserException -> IO (a, Store, RefStore)
    handleUE (UEX b d s r) = case cast b of
      Just b' -> runLazyWith l2 s r >>= \(f, s', r') ->
                 runLazyWith (f (return b')) s' r'
      Nothing -> throw (UEX b d s r)

orElseLazy :: Lazy a -> Lazy a -> Lazy a
orElseLazy l1 l2 = Lazy $ \c s r -> do
  (a, s', r') <- runLazyWith l1 s r `catch` \(UEX _ _ sx rx) ->
    runLazyWith l2 sx rx
  c a s' r'

throwLazy :: Typeable a => Lazy a -> String -> Lazy b
throwLazy l disp = Lazy $ \_ s r -> do
  (v, s', r') <- runLazyWith l s r
  throw (UEX v disp s' r')

instance Applicative Lazy where
  {-# INLINE pure #-}
  pure = pureL
  (<*>) = ap

instance Monad Lazy where
  {-# INLINE (>>=) #-}
  (>>=) = andThen

{-# RULES
"pure/bind"   forall f x. andThen (pureL x) f = f x
  #-}

{-# INLINE[0] pureL #-}
-- | Inlineable implementation of 'pure' for 'Lazy'
pureL :: a -> Lazy a
pureL x = Lazy (\c -> c x)

{-# INLINE[0] andThen #-}
-- | Inlineable implementation of '(>>=)' for 'Lazy'
andThen :: Lazy a -> (a -> Lazy b) -> Lazy b
andThen a k = Lazy (\c s -> fromLazy a (\x -> fromLazy (k x) c) s)

instance MonadFail Lazy where
  fail s = Lazy (\_ _ _ -> fail s)

instance MonadState (Store, RefStore) Lazy where
  get = Lazy (\c s r -> c (s, r) s r)
  put (s, r) = Lazy (\c _ _ -> c () s r)

instance Sharing Lazy where
  type ShareConstraints Lazy a = Shareable Lazy a
  share a = memo (a >>= shareArgs share)

-- | A data type to label and store shared nondeterministic values
-- on an untyped heap.
data Store = Store
  { nextLabel :: !Int
  , heap :: !(M.IntMap Untyped)
  }

-- | An empty storage.
emptyStore :: Store
emptyStore = Store 0 M.empty

-- | Get a new fresh label.
freshLabel :: MonadState Store m => m Int
freshLabel = do
  s <- get
  put (s {nextLabel = nextLabel s + 1})
  return (nextLabel s)

-- | Look up the vaule for a given label in the store.
lookupValue :: MonadState Store m => Int -> m (Maybe a)
lookupValue k = gets (fmap typed . M.lookup k . heap)

-- | Store a given value for later.
storeValue :: MonadState Store m => Int -> a -> m ()
storeValue k v = modify (\s -> s { heap = M.insert k (Untyped v) (heap s) })

{-# INLINE memo #-}
-- | Memorize a nondeterministic value for explicit sharing.
memo :: Lazy a -> Lazy (Lazy a)
memo a =
  Lazy
    (\c1 (Store key heap1) ->
       c1
         (Lazy
            (\c2 s@(Store _ heap2) ->
               case M.lookup key heap2 of
                 Just x -> c2 (typed x) s
                 Nothing ->
                   fromLazy
                     a
                     (\x (Store other heap3) ->
                        c2 x (Store other (M.insert key (Untyped x) heap3)))
                     s))
         (Store (succ key) heap1))

-- | Wrap a typed value in an untyped container.
data Untyped = forall a. Untyped a

untyped :: a -> Untyped
untyped = Untyped

-- | Extract a typed value from an untyped container.
typed :: Untyped -> a
typed (Untyped x) = unsafeCoerce x

data RefStore = RefStore !(M.IntMap Untyped)

emptyRefStore :: RefStore
emptyRefStore = RefStore M.empty

getOrCreateGlobalRefWith :: Int -> a -> Lazy (IORef a)
getOrCreateGlobalRefWith i v = do
  (s, RefStore r) <- get
  case M.lookup i r of
    Just ref -> return (typed ref)
    Nothing  -> do
      ref <- liftIOInLazy $ newIORef v
      put (s, RefStore $ M.insert i (untyped ref) r)
      return ref

writeToRef :: IORef a -> a -> Lazy a
writeToRef ref a = do
  liftIOInLazy $ writeIORef ref a
  return a

readFromRef :: IORef a -> Lazy a
readFromRef ref = do
  liftIOInLazy $ readIORef ref

putStrLazy :: String -> Lazy ()
putStrLazy = liftIOInLazy . putStr
