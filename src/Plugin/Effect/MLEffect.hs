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

This module contains defintions for a strict Standard ML-like effect monad
with support for explicit sharing of computations in call-by-value.
-}
module Plugin.Effect.MLEffect where

import qualified Data.Map.Strict as M
import           Data.IORef
import           Data.Typeable
import           Control.Exception
import           Control.Monad
import           Control.Monad.State ( MonadState(..), MonadIO(..), gets, modify )
import           Unsafe.Coerce

import Plugin.Effect.Classes

-- | A Strict implementation of a monad for nondeterminism
-- with support for explicit sharing.
newtype Strict a = Strict {
    fromStrict :: forall n. (a -> TopStore -> IO n)
               -> TopStore -> IO n
  } deriving Functor

-- | Collect all results into a given nondeterministic data structure.
runStrict :: MonadIO io => Strict n -> io n
runStrict m = liftIO $ fromStrict m (\a _ -> return a) emptyTopStore

liftIOInStrict :: IO a -> Strict a
liftIOInStrict io = Strict (\c r -> io >>= \v -> c v r)

runStrictWith :: MonadIO io
              => Strict n -> TopStore -> io (n, TopStore)
runStrictWith m r = liftIO $ fromStrict m (\a c -> return (a, c)) r

data UserException = forall a. Typeable a => UEX a String TopStore

instance Show UserException where
  show (UEX _ disp _) = disp

instance Exception UserException where

handleStrict :: forall a b. Typeable b
             => Strict a -> Strict (Strict b -> Strict a) -> Strict a
handleStrict l1 l2 = Strict $ \c r -> do
  (a, r') <- runStrictWith l1 r `catch` handleUE
  c a r'
  where
    handleUE :: UserException -> IO (a, TopStore)
    handleUE (UEX b d r) = case cast b of
      Just b' -> runStrictWith l2 r >>= \(f, r') ->
                 runStrictWith (f (return b')) r'
      Nothing -> throw (UEX b d r)

orElseStrict :: Strict a -> Strict a -> Strict a
orElseStrict l1 l2 = Strict $ \c r -> do
  (a, r') <- runStrictWith l1 r `catch` \(UEX _ _ rx) ->
    runStrictWith l2 rx
  c a r'

throwStrict :: Typeable a => Strict a -> String -> Strict b
throwStrict l disp = Strict $ \_ r -> do
  (v, r') <- runStrictWith l r
  throw (UEX v disp r')

instance Applicative Strict where
  {-# INLINE pure #-}
  pure = pureS
  (<*>) = ap

instance Monad Strict where
  {-# INLINE (>>=) #-}
  (>>=) = andThen

{-# RULES
"pure/bind" forall f x. andThen (pureS x) f = f x
  #-}

{-# INLINE[0] pureS #-}
-- | Inlineable implementation of 'pure' for 'Strict'
pureS :: a -> Strict a
pureS x = Strict (\c -> c x)

{-# INLINE[0] andThen #-}
-- | Inlineable implementation of '(>>=)' for 'Strict'
andThen :: Strict a -> (a -> Strict b) -> Strict b
andThen a k = Strict (\c -> fromStrict a (\x -> fromStrict (k x) c))

instance MonadFail Strict where
  fail s = Strict (\_ _ -> fail s)

instance MonadState TopStore Strict where
  get = Strict (\c r -> c r r)
  put r = Strict (\c _ -> c () r)

instance Sharing Strict where
  share a = a >>= return . return
  shareTopLevel = readOrExecuteTop

-- | Wrap a typed value in an untyped container.
data Untyped = forall a. Untyped a

-- | Wrap a typed value in an untyped container.
untyped :: a -> Untyped
untyped = Untyped

-- | Extract a typed value from an untyped container.
typed :: Untyped -> a
typed (Untyped x) = unsafeCoerce x

data TopStore = TopStore !(M.Map (Int, String) Untyped)

emptyTopStore :: TopStore
emptyTopStore = TopStore M.empty

unwrapStore :: TopStore -> M.Map (Int, String) Untyped
unwrapStore (TopStore s) = s

readOrExecuteTop :: (Int, String) -> Strict a -> Strict a
readOrExecuteTop k v = do
  entry <- gets (M.lookup k . unwrapStore)
  case entry of
    Just val -> return (typed val)
    _        -> do val <- v
                   modify (TopStore . M.insert k (untyped val) . unwrapStore)
                   return val

writeToRef :: IORef a -> a -> Strict a
writeToRef ref a = do
  liftIOInStrict $ writeIORef ref a
  return a

readFromRef :: IORef a -> Strict a
readFromRef ref = do
  liftIOInStrict $ readIORef ref

putStrStrict :: String -> Strict ()
putStrStrict = liftIOInStrict . putStr
