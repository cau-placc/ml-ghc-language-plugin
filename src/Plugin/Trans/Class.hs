{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Plugin.Trans.Class
-- Description : Functions to handle lifting of classes
-- Copyright   : (c) Kai-Oliver Prott (2020 - 2023)
-- Maintainer  : kai.prott@hotmail.de
-- This module contains a function to lift class definitions for the plugin and
-- another function to look up the lifted class for a given unlifted class
-- with the type constructor mapping.
module Plugin.Trans.Class
  ( liftClass,
    getLiftedClass,
    ClassLiftingException (..),
  )
where

import Control.Exception
import Control.Monad.State
import Data.Maybe
import GHC.Core.Class
import GHC.Core.SimpleOpt
import GHC.Core.TyCo.Rep
import GHC.Core.Unfold.Make
import GHC.Data.List.SetOps
import GHC.Driver.Config
import GHC.Plugins
import GHC.Types.Demand
import GHC.Types.Id.Make
import Plugin.Trans.Type
import Plugin.Trans.Util
import Plugin.Trans.Var

-- | Exception type when lifting of a class fails.
data ClassLiftingException = ClassLiftingException
  { classWithError :: Class,
    errorReason :: String
  }
  deriving (Eq)

instance Show ClassLiftingException where
  show (ClassLiftingException cls s) =
    "ClassLiftingException "
      ++ show (occNameString (occName (className cls)))
      ++ show s

instance Exception ClassLiftingException

type ClassM = StateT UniqSupply IO

instance MonadUnique ClassM where
  getUniqueSupplyM = do
    us <- get
    let (u1, u2) = splitUniqSupply us
    put u2
    return u1

-- | Lift a class definition and all its functions.
-- Note that this is part of a fixed-point computation, where the
-- 'UniqFM' in the third parameter and the
-- 'TyCon' in the fifth parameter depend on the output of the computation.
liftClass ::
  -- | Compiler flags
  DynFlags ->
  -- | 'Shareable' type constructor
  TyCon ->
  -- | 'Nondet' type constructor
  TyCon ->
  -- | '-->' type constructor
  TyCon ->
  -- | Map of old TyCon's from this module to lifted ones
  UniqFM TyCon TyCon ->
  -- | Map of imported old TyCon's to lifted ones
  TyConMap ->
  -- | Lifted class type constructor
  TyCon ->
  -- | Supply of fresh unique keys
  UniqSupply ->
  -- | Class to be lifted
  Class ->
  -- | Lifted class
  IO Class
liftClass dflags stycon ftycon mtycon tcs tcsM tycon us cls = flip evalStateT us $ mdo
  -- Look up the new type constructors for all super classes
  us1 <- getUniqueSupplyM
  us2 <- getUniqueSupplyM
  superclss <-
    liftIO $
      mapM
        (fmap (replaceTyconTyPure tcs) . replaceTyconTy tcsM)
        (classSCTheta cls)
  let mkShareTy ty = mkTyConApp stycon [mkTyConTy mtycon, ty]
  let stys = catMaybes $ zipWith (\v u -> mkShareable mkShareTy u (Bndr v Required)) (classTyVars cls) (listSplitUniqSupply us1)
  let nms =
        map
          ( \u ->
              let un = uniqFromSupply u
               in mkExternalName
                    un
                    (nameModule (className cls))
                    (mkVarOcc $ "$sel" ++ show un)
                    noSrcSpan
          )
          (listSplitUniqSupply us2)
  let ssels =
        zipWith
          ( \ty n ->
              let ty' =
                    mkForAllTys
                      (map (`Bndr` Inferred) $ classTyVars cls)
                      (FunTy InvisArg Many (mkTyConApp tycon (map mkTyVarTy $ classTyVars cls)) ty)
               in mkExactNameDictSelId n cls' ty' dflags
          )
          stys
          nms
  -- Lift the super class selector functions
  supersel <- mapM (liftSuperSel dflags stycon ftycon mtycon tcs tcsM cls') (classSCSelIds cls)
  -- Lift the associated types of the class
  astypes <- mapM (liftATItem mtycon tcs tcsM cls) (classATItems cls)
  -- Lift all class functions
  classops <-
    mapM
      (liftClassOpItem dflags stycon ftycon mtycon tcs tcsM cls cls')
      (classOpItems cls)
  -- Create the new class from its lifted components
  let cls' =
        mkClass
          (tyConName tycon)
          (classTyVars cls)
          (snd (classTvsFds cls))
          (stys ++ superclss)
          (ssels ++ supersel)
          astypes
          classops
          (classMinimalDef cls)
          tycon
  return cls'

-- | Lift a super class selector function.
liftSuperSel :: DynFlags -> TyCon -> TyCon -> TyCon -> UniqFM TyCon TyCon -> TyConMap -> Class -> Var -> ClassM Var
liftSuperSel dflags stycon ftycon mtycon tcs tcsM cls v = do
  u <- getUniqueM
  us <- getUniqueSupplyM
  -- A super class selector is not lifted like a function.
  -- Instead we just have to update its mentioned type constructors.
  ty' <- lift $ replaceTyconTyPure tcs <$> liftInnerTyParametrized False stycon ftycon (mkTyConTy mtycon) us tcsM (varType v)
  -- Create the new selector id with the correct attributes.
  return (mkExactNameDictSelId (liftName (varName v) u) cls ty' dflags)

-- | Lift a class function.
liftClassOpItem ::
  DynFlags ->
  TyCon ->
  TyCon ->
  TyCon ->
  UniqFM TyCon TyCon ->
  TyConMap ->
  Class ->
  Class ->
  ClassOpItem ->
  ClassM ClassOpItem
liftClassOpItem dflags stycon ftycon mtycon tcs tcsM clsOld clsNew (v, mbdef) = do
  us1 <- getUniqueSupplyM
  us2 <- getUniqueSupplyM
  us3 <- getUniqueSupplyM
  u4 <- getUniqueM
  -- The classOp has type forall clsVars . forall otherVars . (...).
  -- If we were to lift the full type,
  -- we would end up with Shareable constraints on clsVars.
  -- But those are bound by the class definition,
  -- including those constraints raises a type error.
  -- So we first split off as many foralls, as there are variables.
  let varCount = length (classTyVars clsOld)
  let (bndr, liftingType) = splitInvisPiTysN varCount (varType v)
  -- Now we can lift the type.
  bndr' <- liftIO (mapM (replacePiTy stycon ftycon (mkTyConTy mtycon) us3 tcsM) bndr)
  ty' <-
    liftIO $
      replaceTyconTyPure tcs . mkPiTys bndr'
        <$> liftType stycon ftycon (mkTyConTy mtycon) us1 tcsM liftingType
  -- Create the new selector id with the correct attributes.
  let v' = mkExactNameDictSelId (varName v) clsNew ty' dflags
  -- Lift any default implementations
  mbdef' <- maybe (return Nothing) (liftDefaultMeth us2) mbdef
  return (liftVarName v' u4, mbdef')
  where
    liftDefaultMeth _ (n, VanillaDM) = do
      u <- getUniqueM
      return (Just (liftName n u, VanillaDM))
    liftDefaultMeth us2 (n, GenericDM ty) = do
      u <- getUniqueM
      ty' <-
        liftIO $
          replaceTyconTyPure tcs
            <$> liftType stycon ftycon (mkTyConTy mtycon) us2 tcsM ty
      return (Just (liftName n u, GenericDM ty'))

-- | Create a selector identifier with the given name, class and type.
-- Basically copied from ghc package, module 'MkId',
-- but the original looks up the type from the class.
-- This would lead to a deadlock, as the type given in the class is created
-- here in the first place.
mkExactNameDictSelId :: Name -> Class -> Type -> DynFlags -> Id
mkExactNameDictSelId name clas sel_ty dflags =
  mkGlobalId (ClassOpId clas) name sel_ty info
  where
    tycon = classTyCon clas
    sel_names = map idName (classAllSelIds clas)
    new_tycon = isNewTyCon tycon
    dc = head (tyConDataCons tycon)
    tyvars = dataConUserTyVarBinders dc
    n_ty_args = length tyvars
    val_index = assoc "MkId.mkDictSelId" (sel_names `zip` [0 ..]) name
    base_info =
      noCafIdInfo
        `setArityInfo` 1
        `setStrictnessInfo` strict_sig
        `setLevityInfoWithType` sel_ty
    info
      | new_tycon =
          base_info
            `setInlinePragInfo` alwaysInlinePragma
            `setUnfoldingInfo` mkInlineUnfoldingWithArity
              1
              (initSimpleOpts dflags)
              (mkDictSelRhs clas val_index)
      | otherwise =
          base_info `setRuleInfo` mkRuleInfo [rule]
    rule =
      BuiltinRule
        { ru_name =
            fsLit "Class op "
              `appendFS` occNameFS (getOccName name),
          ru_fn = name,
          ru_nargs = n_ty_args + 1,
          ru_try = dictSelRule val_index n_ty_args
        }
    strict_sig = mkClosedStrictSig [arg_dmd] topDiv
    arg_dmd
      | new_tycon = evalDmd
      | otherwise =
          C_1N
            :* Prod
              [ if name == sel_name then evalDmd else absDmd
                | sel_name <- sel_names
              ]

-- | Create an unfolding rule for dictionary selector functions.
-- Basically copied from ghc package, module 'MkId',
-- because we need it for our version of 'mkExactNameDictSelId'.
dictSelRule :: Int -> Arity -> RuleFun
dictSelRule val_index n_ty_args _ id_unf _ args
  | (dict_arg : _) <- drop n_ty_args args,
    Just (_, floats, _, _, con_args) <- exprIsConApp_maybe id_unf dict_arg =
      Just (wrapFloats floats $ getNth con_args val_index)
  | otherwise =
      Nothing

-- | Lift an associated type.
-- Not implemented yet, throws an error when it is used.
liftATItem ::
  TyCon ->
  UniqFM TyCon TyCon ->
  TyConMap ->
  Class ->
  ClassATItem ->
  ClassM ClassATItem
liftATItem _ _ _ cls (ATI _ _) =
  throw (ClassLiftingException cls reason)
  where
    reason = "Type class associated types are not supported by the plugin yet"

-- | Look up the lifted version of a class from the given TyCon map.
getLiftedClass :: Class -> TyConMap -> IO Class
getLiftedClass cls tcs = do
  tc' <- lookupTyConMap GetNew tcs (classTyCon cls)
  case tyConClass_maybe tc' of
    Just c -> return c
    _ ->
      panicAnyUnsafe "New version of TyCon of class is not a class itself" cls
