{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE LambdaCase      #-}
{-|
Module      : Plugin.SMLPlugin.THEval
Description : TemplateHaskell functions to generate wrappers.
Copyright   : (c) Kai-Oliver Prott (2020)
Maintainer  : kai.prott@hotmail.de

This module contains functions to automatically generate the correct wrapper
for functions with an arbitrary arity.
-}
module Plugin.SMLPlugin.THEval (runGeneric, runN) where

import Control.Monad

import Language.Haskell.TH

import Plugin.SMLPlugin.Monad

-- | 'runGeneric' encapsulates a ML computation and runs its
--   results in an IOMonad. This encapsulation can handle
--   "simple" higher-order functions, but it requires the type of the
--   encapsulated function to be known at compile-time,
--   among other stage TemplateHaskell stage restrictions.
--   Use the less convenient 'runN', if the type is unknown at compile-time.
--
--   Examples:
--   >>> $(runGeneric 'someNullaryFunction)
--   >>> $(runGeneric 'someUnaryFunction  ) arg1
runGeneric :: Name -> Q Exp
runGeneric fname = do
  ty <- reify fname >>= \case
    VarI     _ ty _ -> return ty
    ClassOpI _ ty _ -> return ty
    _               -> fail "Only functions can be captured"
  argsT <- collectArgs ty
  vs <- replicateM (length argsT) (newName "x")
  ev <- genEval fname (zip vs argsT)
  if null vs
    then return ev
    else return (LamE (map VarP vs) ev)

-- | 'runN' encapsulates a ML computation and runs its
--   results in an IOMonad.
--   This encapsulation requires its user to specify the arity of the wrapped
--   function. It also cannot handle any higher-order functions.
--   Use the more convenient 'runGeneric' for a better interface,
--   unless the type of the wrapped function is not known at compile-time.

--   Examples:
--   >>> $(runN 0) someNullaryFunction
--   >>> $(runN 1) someUnaryFunction   arg1
runN :: Int -> Q Exp
runN n = do
  fname <- newName "f"
  vs <- replicateM n (newName "x")
  -- type does not matter, as long as it is not a nondeterministic function
  ev <- genEval fname (map (,WildCardT) vs)
  return (LamE (map VarP (fname : vs)) ev)

-- | Deconstruct a lifted type to collect its arguments.
collectArgs :: Type -> Q [Type]
collectArgs (AppT (AppT ArrowT ty1) ty2) = (ty1 :) <$> collectArgs ty2
collectArgs (ForallT      _ _ ty) = collectArgs ty
collectArgs (ForallVisT     _ ty) = collectArgs ty
collectArgs (AppT  (ConT nm) ty2)
  | ndName == nm                  = collectArgs ty2
collectArgs (AppKindT       ty _) = collectArgs ty
collectArgs (SigT           ty _) = collectArgs ty
collectArgs (ParensT          ty) = collectArgs ty
collectArgs (ImplicitParamT  _ _) = fail "Implicit params are not supported"
collectArgs _                     = return []

-- | Generate the 'runEffect' part of a wrapper for a given
-- wrapped function and list of arguments.
genEval :: Name -> [(Name, Type)] -> Q Exp
genEval fname [] = do
  rEff  <- [| runEffectNF |]
  return (AppE rEff (VarE fname))
genEval fname args = do
  rEff  <- [| \inn f -> runEffectNF (f >>= inn) |]
  inner <- genHelp args
  return (foldl AppE rEff [inner, VarE fname])
  where
    genHelp :: [(Name, Type)] -> Q Exp
    genHelp []            = [| return |]
    genHelp ((v,_):rest) = do
      x <- newName "f"
      ex <- [| \inn vx vv -> vx (liftE (return vv)) >>= inn |]
      inner <- genHelp rest
      let lam = foldl AppE ex [inner, VarE x, VarE v]
      return (LamE [VarP x] lam)

-- | Name of the monad 'SML' used in the lifting.
ndName :: Name
ndName = ''SML
