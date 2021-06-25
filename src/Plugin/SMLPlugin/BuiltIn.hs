{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE Strict                 #-}
{-# LANGUAGE MagicHash              #-}
{-# OPTIONS_GHC -Wno-orphans        #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}
{-|
Module      : Plugin.CurryPlugin.BuiltIn
Description : Built-In functions, types and type classes
Copyright   : (c) Kai-Oliver Prott (2020)
License     : BSD-3 Clause
Maintainer  : kai.prott@hotmail.de

This module contains lifted replacements for data types, functions
and type classes for Haskell's default Prelude.
This module is not supposed to be imported by users, please import
'Plugin.CurryPlugin.Prelude' instead.
-}
module Plugin.SMLPlugin.BuiltIn where

import qualified Prelude                as P
import           Prelude                     ( ($), Int, Integer, Char
                                             , Float, Double
                                             , Bool(..), Ordering(..) )
import qualified GHC.Real               as P
import qualified GHC.Prim               as P
import qualified GHC.Int                as P
import           Control.Monad.Trans.Class
import           Unsafe.Coerce
import           GHC.Types                   ( RuntimeRep, Multiplicity )
import           Data.IORef
import           Data.Typeable

import Plugin.SMLPlugin.Monad
import Plugin.Effect.Classes (Shareable(..))


-- | This is a lifted version of the unrestricted function type constructor
type (:->) r s = (->)

-- | This is a lifted version of the restricted function type constructor
type (:-->) m r s = (->)

-- | Alias for Shareable constraint specialized to the SML monad.
type ShareableN a = Shareable SML a

-- * Some IO Effects

-- | Output a String
putStr :: SML (StringND --> ())
putStr = rtrnFunc $ \s -> nf s P.>>= \s' ->
  runIO (P.putStr s')


-- | Output a String with a new line
putStrLn :: SML (StringND --> ())
putStrLn = rtrnFunc $ \s -> nf s P.>>= \s' ->
  runIO (P.putStrLn (s' :: P.String))

-- | Output a Showable value with a new line
print :: (ShowND a, ShareableN a) => SML (a --> ())
print = rtrnFunc $ \a -> show P.>>= \(Func f) -> nf (f a) P.>>= \s' ->
  runIO (P.putStrLn (s' :: P.String))

getChar :: SML Char
getChar = runIO P.getChar

getLine :: SML StringND
getLine = liftE (runIO P.getLine)

raise :: (ShowND a, Typeable a, Shareable SML a, Shareable SML b)
      => SML (a --> b)
raise = rtrnFunc $ \a -> show P.>>= \(Func f) -> nf (f a) P.>>= \s ->
  throwStrict a s

handle :: (Typeable b, Shareable SML a, Shareable SML b)
       => SML (a --> (b --> a) --> a)
handle = rtrnFunc $ \act -> rtrnFunc $ \cse ->
  handleStrict act (P.fmap (\(Func f) -> f) cse)

orElse :: (Typeable a, Shareable SML a)
       => SML (a --> a --> a)
orElse = rtrnFunc $ \a1 -> rtrnFunc $ \a2 ->
  orElseStrict a1 a2

-- * Lifted list type and internal instances

-- | Lifted defintion for Haskell's default list type '[]'
data ListND a = Nil | Cons (SML a) (SML (ListND a))

-- | Lifted smart constructor for 'Nil'
nil :: SML (ListND a)
nil = P.return Nil

-- | Lifted smart constructor for 'Cons'
cons :: SML (a --> ListND a --> ListND a)
cons = rtrnFunc $ \a -> rtrnFunc $ \as -> P.return (Cons a as)

-- | Shareable instance for lists.
instance Shareable SML a => Shareable SML (ListND a) where
  shareArgs Nil         = P.return Nil
  shareArgs (Cons x xs) = Cons P.<$> share x P.<*> share xs

-- | Normalform instance for lists
instance Normalform SML a1 a2 => Normalform SML (ListND a1) [a2] where
  nf mxs = mxs P.>>= \case
    Nil       -> P.return []
    Cons x xs -> (:) P.<$> nf x P.<*> nf xs
  liftE mxs = mxs P.>>= \case
    []   -> P.return Nil
    x:xs -> Cons P.<$> P.return (liftE (P.return x))
                 P.<*> P.return (liftE (P.return xs))

-- * Lifted tuple types and internal instances

-- | Lifted defintion for Haskell's 2-ary tuple '(,)'
data Tuple2ND a b = Tuple2 (SML a) (SML b)

-- | Selector for the first component of a lifted 2-ary tuple
fst :: SML (Tuple2ND a b --> a)
fst = rtrnFunc $ \t -> t P.>>= \(Tuple2 a _) -> a

-- | Selector for the second component of a lifted 2-ary tuple
snd :: SML (Tuple2ND a b --> b)
snd = rtrnFunc $ \t -> t P.>>= \(Tuple2 _ b) -> b

-- | Shareable instance for 2-ary tuple
instance (Shareable SML a, Shareable SML b) =>
  Shareable SML (Tuple2ND a b) where
    shareArgs (Tuple2 a b) = Tuple2 P.<$> share a P.<*> share b

-- | Normalform instance for 2-ary tuple
instance (Normalform SML a1 a2, Normalform SML b1 b2) =>
  Normalform SML (Tuple2ND a1 b1) (a2, b2) where
    nf mxs = mxs P.>>= \(Tuple2 a b) -> (,) P.<$> nf a P.<*> nf b
    liftE mxs = mxs P.>>= \(a, b) -> Tuple2 P.<$> P.return (liftE (P.return a))
                                            P.<*> P.return (liftE (P.return b))

-- * Other lifted types and internal instances

-- | Lifted defintion for Haskell's 'String' type
type StringND = ListND P.Char

-- | Lifted defintion for Haskell's 'Ratio' type
data RatioND a = !(SML a) :% !(SML a)

-- | Shareable instance for Ratios
instance (Shareable SML a) =>
  Shareable SML (RatioND a) where
    shareArgs (a :% b) = (:%) P.<$> share a P.<*> share b

-- | Normalform instance for Ratios
instance (Normalform SML a1 a2) =>
  Normalform SML (RatioND a1) (P.Ratio a2) where
    nf mxs = mxs P.>>= \(a :% b) -> (P.:%) P.<$> nf a P.<*> nf b
    liftE mxs = mxs P.>>= \(a P.:% b) ->
      (:%) P.<$> P.return (liftE (P.return a))
           P.<*> P.return (liftE (P.return b))

-- | Lifted defintion for Haskell's 'Rational' type
type RationalND = RatioND Integer


-- * Lifted functions

-- $liftedFunctions
-- The pre-lifted functions are used to desugar
-- do-notation, (list) comprehensions or to replace their Haskell counterpart
-- in derived instances.

-- | Function to use for pattern match failures
-- Pattern match failure is translated to a failed for Curry,
-- ignoring the string.
pE :: ShareableN a => SML (ListND Char --> a)
pE = rtrnFunc (\l -> nf l P.>>= \s -> SML $ lift $ lift $ P.fail s)

-- | Lifted identity function
id :: SML (a --> a)
id = rtrnFunc P.id

-- | Lifted logical negation
not :: SML (Bool --> Bool)
not = liftSML1 P.not

-- Note: In order to be able to keep all type-applications
-- of the original code for the following "primops",
-- we introduce the same number and order of type variables,
-- even if they are unused

-- | Lifted seq operator to force evaluation. Forces the effect and value.
seq :: forall (k :: RuntimeRep) a b. SML (a --> b --> b)
seq = rtrnFunc $ \a -> rtrnFunc $ \b ->
  (a P.>>= \a' -> P.seq a' b)

-- | Lifted function to desugar left sections.
leftSection :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                      (n :: Multiplicity) a b.
               SML ((a --> b) --> a --> b)
leftSection = rtrnFunc $ \f -> rtrnFunc $ \a ->
  f `app` a

-- | Lifted function to desugar right sections.
rightSection :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep) (r3 :: RuntimeRep)
                       (n1 :: Multiplicity) (n2 :: Multiplicity) a b c.
                SML ((a --> b --> c) --> b --> a --> c)
rightSection = rtrnFunc $ \f -> rtrnFunc $ \b -> rtrnFunc $ \a ->
  f `app` a `app` b

-- | Lifted const function
const :: SML (a --> b --> a)
const = rtrnFunc $ \a -> rtrnFunc $ \_ -> a

-- | Lifted logical and
(&&) :: SML (Bool --> Bool --> Bool)
(&&) =  rtrnFunc $ \a1 -> rtrnFunc $ \a2 -> a1 P.>>= \case
  False -> P.return False
  True  -> a2

-- | Lifted guard function used to desugar monad comprehensions
guard :: (AlternativeND f, forall x . ShareableN x => ShareableN (f x))
      => SML (Bool --> f ())
guard = rtrnFunc $ \b -> b P.>>= \case
  True  -> app pure (P.return ())
  False -> empty

-- | Lifted append function for lists
append :: ShareableN a => SML (ListND a --> ListND a --> ListND a)
append = rtrnFunc $ \xs -> rtrnFunc $ \ys -> xs P.>>= \case
  Nil       -> ys
  Cons a as -> P.return (Cons a (apply2 append as ys))

-- | Lifted concatMap function for lists
concatMap :: (ShareableN a, ShareableN b)
          => SML ((a --> ListND b) --> ListND a --> ListND b)
concatMap = rtrnFunc $ \f -> rtrnFunc $ \xs -> xs P.>>= \case
  Nil       -> P.return Nil
  Cons a as -> apply2 append (app f a) (apply2 concatMap f as)

-- | Lifted map function for lists
map :: SML ((a --> b) --> ListND a --> ListND b)
map = rtrnFunc $ \f' -> share f' P.>>= \f ->
  rtrnFunc $ \xs -> xs P.>>= \case
  Nil       -> P.return Nil
  Cons a as -> P.return (Cons (app f a) (apply2 map f as))

-- | Lifted coercion function to replace coercion in newtype-derived instances
-- We need to introduce this unused dummy k,
-- because we replace Data.Coerce.coerce (which has this k).
coerce :: forall (k :: RuntimeRep) a b. (ShareableN a, ShareableN b)
       => SML (a --> b)
coerce = rtrnFunc $ \a -> a P.>>= \a' -> P.return (unsafeCoerce a')

-- | Lifted equality test for strings
eqString :: SML (StringND --> StringND --> Bool)
eqString = (==)

(<#) :: SML (Int --> Int --> Int)
(<#) = rtrnFunc $ \a -> rtrnFunc $ \b ->
  a P.>>= \ (P.I# a') -> b P.>>= \ (P.I# b') ->
   P.return (P.I# (a' P.<# b'))

(==#) :: SML (Int --> Int --> Int)
(==#) = rtrnFunc $ \a -> rtrnFunc $ \b ->
  a P.>>= \ (P.I# a') -> b P.>>= \ (P.I# b') ->
  P.return (P.I# (a' P.==# b'))

-- |  Lifted composition operator for functions
(.) :: (ShareableN a, ShareableN b, ShareableN c)
    => SML ((b --> c) --> (a --> b) --> a --> c)
(.) = rtrnFunc $ \f1 -> rtrnFunc $ \f2 -> rtrnFunc $ \a ->
  app f1 (app f2 a)

-- * Lifted Show type class, instances and functions

-- | Lifted ShowS type
type ShowSND = StringND --> StringND

-- | Lifted Show type class
class ShowND a where
  {-# MINIMAL showsPrec | show #-}
  showsPrec :: SML (Int --> a --> ShowSND)
  showsPrec = rtrnFunc $ \_ -> rtrnFunc $ \x -> rtrnFunc $ \s ->
    apply2 append (app show x) s

  show :: SML (a --> StringND)
  show = rtrnFunc $ \x -> apply2 shows x (P.return Nil)

  showList :: SML (ListND a --> ShowSND)
  showList = rtrnFunc $ \ls -> rtrnFunc $ \s -> apply3 showsList__ shows ls s

showsList__ :: SML ((a --> ShowSND) --> ListND a --> ShowSND)
showsList__ = rtrnFunc $ \showx -> rtrnFunc $ \list -> rtrnFunc $ \s ->
  list P.>>= \case
    Nil       -> apply2 append (liftE (P.return "[]")) s
    Cons x xs ->
      P.return (Cons (P.return '[') (apply2 showx x (apply3 showl showx xs s)))
  where
    showl = rtrnFunc $ \showx -> rtrnFunc $ \list -> rtrnFunc $ \s ->
      list P.>>= \case
        Nil       ->
          P.return (Cons (P.return ']') s)
        Cons y ys ->
          P.return (Cons (P.return ',')
            (apply2 showx y (apply3 showl showx ys s)))

shows :: ShowND a => SML (a --> ShowSND)
shows = app showsPrec (P.return 0)

showString :: SML (StringND --> ShowSND)
showString = append

showCommaSpace :: SML ShowSND
showCommaSpace = app showString (liftE (P.return ", "))

showSpace :: SML ShowSND
showSpace =  app showString (liftE (P.return " "))

showParen :: SML (Bool --> ShowSND --> ShowSND)
showParen = rtrnFunc $ \b -> rtrnFunc $ \s -> b P.>>= \case
  True  -> apply2 (.) (app showString (liftE (P.return "(")))
          (apply2 (.) s (app showString (liftE (P.return ")"))))
  False -> s

instance ShowND Bool where
  show = rtrnFunc $ \x -> liftE (P.show P.<$> x)

instance ShowND () where
  show = rtrnFunc $ \x -> liftE (P.show P.<$> x)

instance ShowND Int where
  show = rtrnFunc $ \x -> liftE (P.show P.<$> x)

instance ShowND Integer where
  show = rtrnFunc $ \x -> liftE (P.show P.<$> x)

instance ShowND Float where
  show = rtrnFunc $ \x -> liftE (P.show P.<$> x)

instance ShowND Double where
  show = rtrnFunc $ \x -> liftE (P.show P.<$> x)

instance ShowND Char where
  show = rtrnFunc $ \x -> liftE (P.show P.<$> x)
  showList = rtrnFunc $ \ls -> rtrnFunc $ \s ->
    liftE (P.showList P.<$> nf ls P.<*> nf s)

instance (ShowND a, ShareableN a) => ShowND (ListND a) where
  show = rtrnFunc $ \xs -> apply2 showList xs (P.return Nil)

-- * Lifted Eq type class, instances and functions

-- | Lifted Eq type class
class EqND a where
  (==) :: SML (a --> a --> Bool)
  (==) = rtrnFunc $ \a1 -> rtrnFunc $ \a2 -> app not (apply2 (/=) a1 a2)

  (/=) :: SML (a --> a --> Bool)
  (/=) = rtrnFunc $ \a1 -> rtrnFunc $ \a2 -> app not (apply2 (==) a1 a2)

instance EqND Bool where
  (==) = liftSML2 (P.==)
  (/=) = liftSML2 (P./=)

instance EqND () where
  (==) = rtrnFunc $ \_ -> rtrnFunc $ \_ -> P.return True
  (/=) = rtrnFunc $ \_ -> rtrnFunc $ \_ -> P.return False

instance EqND Int where
  (==) = liftSML2 (P.==)
  (/=) = liftSML2 (P./=)

instance EqND Integer where
  (==) = liftSML2 (P.==)
  (/=) = liftSML2 (P./=)

instance EqND Float where
  (==) = liftSML2 (P.==)
  (/=) = liftSML2 (P./=)

instance EqND Double where
  (==) = liftSML2 (P.==)
  (/=) = liftSML2 (P./=)

instance EqND Char where
  (==) = liftSML2 (P.==)
  (/=) = liftSML2 (P./=)

instance EqND (IORef a) where
  (==) = liftSML2 (P.==)
  (/=) = liftSML2 (P./=)

instance (EqND a, ShareableN a) => EqND (ListND a) where
  (==) = rtrnFunc $ \a1 -> rtrnFunc $ \a2 -> a1 P.>>= \case
    Nil       -> a2 P.>>= \case
      Nil       -> P.return True
      Cons _ _  -> P.return False
    Cons x xs -> a2 P.>>= \case
      Nil       -> P.return False
      Cons y ys -> eqOn x y xs ys

instance (EqND a, EqND b, ShareableN a, ShareableN b) =>
  EqND (Tuple2ND a b) where
  (==) = rtrnFunc $ \x1 -> rtrnFunc $ \x2 -> do
    (Tuple2 a1 b1) <- x1
    (Tuple2 a2 b2) <- x2
    eqOn a1 a2 b1 b2

instance (EqND a, ShareableN a) => EqND (RatioND a) where
  (==) = rtrnFunc $ \x1 -> rtrnFunc $ \x2 -> do
    (a1 :% b1) <- x1
    (a2 :% b2) <- x2
    eqOn a1 a2 b1 b2

eqOn :: (EqND a1, EqND a2, ShareableN a1, ShareableN a2)
     => SML a1 -> SML a1 -> SML a2 -> SML a2 -> SML Bool
eqOn x y xs ys = apply2 (&&) (apply2 (==) x y) (apply2 (==) xs ys)

-- * Lifted Ord type class, instances and functions

-- | Lifted Ord type class
class EqND a => OrdND a where
  {-# MINIMAL compare | (<=) #-}
  compare :: SML (a --> a --> Ordering)
  compare = rtrnFunc $ \a1 -> rtrnFunc $ \a2 ->
    apply2 (==) a1 a2 P.>>= \b1 -> if b1
      then P.return EQ
      else apply2 (<=) a1 a2 P.>>= \b2 -> if b2
        then P.return LT
        else P.return GT

  (<) :: SML (a --> a --> Bool)
  (<) = rtrnFunc $ \a1 -> rtrnFunc $ \a2 ->
    apply2 compare a1 a2 P.>>= \case
      LT -> P.return True
      _  -> P.return False

  (<=) :: SML (a --> a --> Bool)
  (<=) = rtrnFunc $ \a1 -> rtrnFunc $ \a2 ->
    apply2 compare a1 a2 P.>>= \case
      GT -> P.return False
      _  -> P.return True

  (>) :: SML (a --> a --> Bool)
  (>) = rtrnFunc $ \a1 -> rtrnFunc $ \a2 ->
    apply2 compare a1 a2 P.>>= \case
      GT -> P.return True
      _  -> P.return False

  (>=) :: SML (a --> a --> Bool)
  (>=) = rtrnFunc $ \a1 -> rtrnFunc $ \a2 ->
    apply2 compare a1 a2 P.>>= \case
      LT -> P.return False
      _  -> P.return True

  -- This default implementation is replaced at compile-time with maxDefault
  max :: SML (a --> a --> a)
  max = P.undefined

  -- This default implementation is replaced at compile-time with minDefault
  min :: SML (a --> a --> a)
  min = P.undefined

maxDefault :: (OrdND a, ShareableN a) => SML (a --> a --> a)
maxDefault = rtrnFunc $ \a1 -> rtrnFunc $ \a2 ->
  share a1 P.>>= \a1' -> share a2 P.>>= \a2' ->
  apply2 (>=) a1' a2' P.>>= \case
    True -> a1'
    _    -> a2'

minDefault :: (OrdND a, ShareableN a) => SML (a --> a --> a)
minDefault = rtrnFunc $ \a1 -> rtrnFunc $ \a2 ->
  share a1 P.>>= \a1' -> share a2 P.>>= \a2' ->
  apply2 (<=) a1' a2' P.>>= \case
    True -> a1'
    _    -> a2'

instance OrdND Bool where
  compare = liftSML2 P.compare

instance OrdND () where
  compare = rtrnFunc $ \_ -> rtrnFunc $ \_ -> P.return EQ

instance OrdND Int where
  compare = liftSML2 P.compare

instance OrdND Integer where
  compare = liftSML2 P.compare

instance OrdND Float where
  compare = liftSML2 P.compare

instance OrdND Double where
  compare = liftSML2 P.compare

instance (OrdND a, ShareableN a) => OrdND (ListND a) where
  compare = rtrnFunc $ \x -> rtrnFunc $ \y ->
    x P.>>= \x' -> y P.>>= \y' -> case (x', y') of
      (Nil      , Nil      ) -> P.return EQ
      (Nil      , Cons _ _ ) -> P.return LT
      (Cons _ _ , Nil      ) -> P.return GT
      (Cons a as, Cons b bs) -> apply2 compare a b P.>>= \case
        EQ -> apply2 compare as bs
        o  -> P.return o

instance (OrdND a, OrdND b, ShareableN a, ShareableN b) =>
  OrdND (Tuple2ND a b) where
  compare = rtrnFunc $ \x -> rtrnFunc $ \y ->
    x P.>>= \x' -> y P.>>= \y' -> case (x', y') of
      (Tuple2 a1 b1, Tuple2 a2 b2) -> apply2 compare a1 a2 P.>>= \case
        EQ -> apply2 compare b1 b2
        o  -> P.return o

-- * Lifted Num type class, instances and functions

-- | Lifted Num type class
class NumND a where
  (+) :: SML (a --> a --> a)
  (-) :: SML (a --> a --> a)
  (-) = rtrnFunc $ \a -> rtrnFunc $ \b ->
    (+) `app` a `app` (negate `app` b)
  (*) :: SML (a --> a --> a)
  negate :: SML (a --> a)
  negate = rtrnFunc $ \a ->
    (-) `app` (fromInteger `app` (P.return 0)) `app` a
  abs    :: SML (a --> a)
  signum :: SML (a --> a)
  fromInteger :: SML (P.Integer --> a)

instance NumND Int where
  (+) = liftSML2 (P.+)
  (-) = liftSML2 (P.-)
  (*) = liftSML2 (P.*)
  negate = liftSML1 P.negate
  abs    = liftSML1 P.abs
  signum = liftSML1 P.signum
  fromInteger = liftSML1 P.fromInteger

instance NumND Integer where
  (+) = liftSML2 (P.+)
  (-) = liftSML2 (P.-)
  (*) = liftSML2 (P.*)
  negate = liftSML1 P.negate
  abs    = liftSML1 P.abs
  signum = liftSML1 P.signum
  fromInteger = liftSML1 P.fromInteger

instance NumND Float where
  (+) = liftSML2 (P.+)
  (-) = liftSML2 (P.-)
  (*) = liftSML2 (P.*)
  negate = liftSML1 P.negate
  abs    = liftSML1 P.abs
  signum = liftSML1 P.signum
  fromInteger = liftSML1 P.fromInteger

instance NumND Double where
  (+) = liftSML2 (P.+)
  (-) = liftSML2 (P.-)
  (*) = liftSML2 (P.*)
  negate = liftSML1 P.negate
  abs    = liftSML1 P.abs
  signum = liftSML1 P.signum
  fromInteger = liftSML1 P.fromInteger

-- * Lifted Fractional type class, instances and functions

-- | Lifted Fractional type class
class NumND a => FractionalND a where
  {-# MINIMAL fromRational, (recip | (/)) #-}

  (/) :: SML (a --> a --> a)
  (/) = rtrnFunc $ \x -> rtrnFunc $ \y -> apply2 (*) x  (recip `app` y)

  recip :: SML (a --> a)
  recip = rtrnFunc $ \x -> apply2 (/) (fromInteger `app` (P.return 1)) x

  fromRational :: SML (RationalND --> a)

instance FractionalND Float where
  (/) = liftSML2 (P./)
  fromRational = rtrnFunc $ \r -> P.fromRational P.<$> nf r

instance FractionalND Double where
  (/) = liftSML2 (P./)
  fromRational = rtrnFunc $ \r -> P.fromRational P.<$> nf r

-- * Lifted Real type class, instances and functions

-- | Lifted Real type class
class (NumND a, OrdND a) => RealND a where
  toRational :: SML (a --> RationalND)

instance RealND Int where
  toRational = rtrnFunc $ \i -> P.return (app toInteger i :% (P.return 1))

instance RealND Integer where
  toRational = rtrnFunc $ \i -> P.return (i :% (P.return 1))

instance RealND Float where
  toRational = rtrnFunc $ \f -> liftE (P.toRational P.<$> f)

instance RealND Double where
  toRational = rtrnFunc $ \d -> liftE (P.toRational P.<$> d)

-- * Lifted Integral type class, instances and functions

-- | Lifted Integral type class
class (RealND a, EnumND a) => IntegralND a where
  quot      :: SML (a --> a --> a)
  rem       :: SML (a --> a --> a)
  div       :: SML (a --> a --> a)
  mod       :: SML (a --> a --> a)
  quotRem   :: SML (a --> a --> Tuple2ND a a)
  divMod    :: SML (a --> a --> Tuple2ND a a)
  toInteger :: SML (a --> Integer)

  quot   = rtrnFunc $ \n -> rtrnFunc $ \d -> app fst (apply2 quotRem n d)
  rem    = rtrnFunc $ \n -> rtrnFunc $ \d -> app snd (apply2 quotRem n d)
  div    = rtrnFunc $ \n -> rtrnFunc $ \d -> app fst (apply2 divMod n d)
  mod    = rtrnFunc $ \n -> rtrnFunc $ \d -> app snd (apply2 divMod n d)

  -- This default implementation is replaced at compile-time with divModDefault
  divMod = P.undefined

divModDefault :: (IntegralND a, ShareableN a)
              => SML (a --> a --> Tuple2ND a a)
divModDefault = rtrnFunc $ \n' -> rtrnFunc $ \d' ->
  share n' P.>>= \n -> share d' P.>>= \d ->
  let qr' = apply2 quotRem n d
  in share qr' P.>>= \qr ->
     qr P.>>= \(Tuple2 q r) -> apply2 (==) (app signum r)
                                           (app negate (app signum d))
        P.>>= \b -> if b
          then P.return (Tuple2 (apply2 (-) q
                                   (app fromInteger (P.return 1)))
                                   (apply2 (+) r d))
          else qr

instance IntegralND Int where
  quot = liftSML2 (P.quot)
  rem  = liftSML2 (P.rem)
  div  = liftSML2 (P.div)
  mod  = liftSML2 (P.mod)

  quotRem = rtrnFunc $ \a1 -> rtrnFunc $ \a2 -> liftE
    (P.quotRem P.<$> a1 P.<*> a2)
  divMod = rtrnFunc $ \a1 -> rtrnFunc $ \a2 -> liftE
    (P.divMod P.<$> a1 P.<*> a2)

  toInteger = liftSML1 (P.toInteger)

instance IntegralND Integer where
  quot = liftSML2 (P.quot)
  rem  = liftSML2 (P.rem)
  div  = liftSML2 (P.div)
  mod  = liftSML2 (P.mod)

  quotRem = rtrnFunc $ \a1 -> rtrnFunc $ \a2 -> liftE
    (P.quotRem P.<$> a1 P.<*> a2)
  divMod = rtrnFunc $ \a1 -> rtrnFunc $ \a2 -> liftE
    (P.divMod P.<$> a1 P.<*> a2)

  toInteger = rtrnFunc P.id

-- * Lifted Monad & Co type classes and instances

infixl 1 >>=, >>
infixl 4 <$, <*, *>, <*>
-- | Lifted Functor type class
class FunctorND f where
  fmap :: (ShareableN a, ShareableN b) => SML ((a --> b) --> f a --> f b)
  (<$) :: (ShareableN a, ShareableN b) => SML (a --> f b --> f a)
  (<$) = rtrnFunc $ \a -> rtrnFunc $ \f ->
    apply2 fmap (app const a) f

instance FunctorND (Tuple2ND a) where
  fmap = rtrnFunc $ \f -> rtrnFunc $ \t -> t P.>>= \case
    Tuple2 a b -> P.return (Tuple2 a (app f b))

instance FunctorND ListND where
  fmap = rtrnFunc $ \f -> rtrnFunc $ \l -> l P.>>= \case
    Nil       -> P.return Nil
    Cons x xs -> P.return (Cons (app f x) (apply2 fmap f xs))

-- | Lifted Applicative type class
class FunctorND f => ApplicativeND f where
  pure :: ShareableN a => SML (a --> f a)

  (<*>) :: (ShareableN a, ShareableN b) => SML (f (a --> b) --> f a --> f b)
  (<*>) = rtrnFunc $ \f -> rtrnFunc $ \a ->
    apply3 liftA2 (liftSML1 P.id) f a

  liftA2 :: (ShareableN a, ShareableN b, ShareableN c)
         => SML ((a --> b --> c) --> f a --> f b --> f c)
  liftA2 = rtrnFunc $ \f -> rtrnFunc $ \a -> rtrnFunc $ \b ->
    apply2 (<*>) (apply2 fmap f a) b

  (*>) :: (ShareableN a, ShareableN b) => SML (f a --> f b --> f b)
  (*>) = rtrnFunc $ \a -> rtrnFunc $ \b ->
    apply3 liftA2 (liftSML2 (P.flip P.const)) a b

  (<*) :: (ShareableN a, ShareableN b) => SML (f a --> f b --> f a)
  (<*) = rtrnFunc $ \a -> rtrnFunc $ \b ->
    apply3 liftA2 const a b
  {-# MINIMAL pure, ((<*>) | liftA2) #-}

instance ApplicativeND ListND where
  pure = rtrnFunc $ \a -> P.return (Cons a (P.return Nil))
  (<*>) = rtrnFunc $ \fs -> rtrnFunc $ \as ->
    apply2 concatMap (rtrnFunc $ \a ->
    apply2 fmap      (rtrnFunc $ \f -> app f a) fs) as

  -- | Lifted Alternative type class
class ApplicativeND f => AlternativeND f where
  empty :: ShareableN a => SML (f a)
  (<|>) :: ShareableN a => SML (f a --> f a --> f a)
  some  :: ShareableN a => SML (f a --> f (ListND a))
  some = rtrnFunc $ \v ->
    let many_v = apply2 (<|>) some_v (app pure (P.return Nil))
        some_v = apply3 liftA2 cons v many_v
    in some_v
  many  :: ShareableN a => SML (f a --> f (ListND a))
  many = rtrnFunc $ \v ->
    let many_v = apply2 (<|>) some_v (app pure (P.return Nil))
        some_v = apply3 liftA2 cons v many_v
    in many_v

instance AlternativeND ListND where
  empty = nil
  (<|>) = append

-- | Lifted Monad type class
class ApplicativeND m => MonadND m where
  (>>=) :: (ShareableN a, ShareableN b) => SML (m a --> (a --> m b) --> m b)
  (>>)  :: (ShareableN a, ShareableN b) => SML (m a --> m b --> m b)
  (>>) = rtrnFunc $ \a -> rtrnFunc $ \b ->
    apply2 (>>=) a (rtrnFunc (P.const b))
  return :: ShareableN a => SML (a --> m a)
  return = pure
  {-# MINIMAL (>>=) #-}

instance MonadND ListND where
  (>>=) = rtrnFunc $ \a -> rtrnFunc $ \f -> a P.>>= \case
    Nil       -> P.return Nil
    Cons x xs -> apply2 append (app f x) (apply2 (>>=) xs f)

-- | Lifted MonadFail type class
class MonadND m => MonadFailND m where
  fail :: ShareableN a => SML (StringND --> m a)

instance MonadFailND ListND where
  fail = rtrnFunc $ \_ -> P.return Nil

-- * Lifted Enum type class, instances and functions

-- | Lifted Enum type class
class EnumND a where
  succ :: SML (a --> a)
  succ = rtrnFunc $ \a ->
    app toEnum (apply2 (+) (P.return 1) (app fromEnum a))
  pred :: SML (a --> a)
  pred = rtrnFunc $ \a ->
    app toEnum (apply2 (-) (P.return 1) (app fromEnum a))

  toEnum   :: SML (Int --> a)
  fromEnum :: SML (a --> Int)

  enumFrom       :: SML (a             --> ListND a)
  enumFrom       = rtrnFunc $ \x1 ->
    apply2 map toEnum (app enumFrom
      (app fromEnum x1))

  enumFromThen   :: SML (a --> a       --> ListND a)
  enumFromThen   = rtrnFunc $ \x1 -> rtrnFunc $ \x2 ->
    apply2 map toEnum (apply2 enumFromThen
      (app fromEnum x1) (app fromEnum x2))

  enumFromTo     :: SML (a       --> a --> ListND a)
  enumFromTo     = rtrnFunc $ \x1 ->                   rtrnFunc $ \x3 ->
    apply2 map toEnum (apply2 enumFromTo
      (app fromEnum x1)                      (app fromEnum x3))

  enumFromThenTo :: SML (a --> a --> a --> ListND a)
  enumFromThenTo = rtrnFunc $ \x1 -> rtrnFunc $ \x2 -> rtrnFunc $ \x3 ->
    apply2 map toEnum (apply3 enumFromThenTo
      (app fromEnum x1) (app fromEnum x2) (app fromEnum x3))

instance EnumND Int where
  succ = app (+) (P.return 1)
  pred = app (-) (P.return 1)

  toEnum   = id
  fromEnum = id

  enumFrom = rtrnFunc $ \x1 ->
    x1 P.>>= \v1 ->
    liftE (P.return (P.enumFrom v1))

  enumFromThen = rtrnFunc $ \x1 -> rtrnFunc $ \x2 ->
    x1 P.>>= \v1 -> x2 P.>>= \v2 ->
    liftE (P.return (P.enumFromThen v1 v2))

  enumFromTo = rtrnFunc $ \x1 -> rtrnFunc $ \x3 ->
    x1 P.>>= \v1 -> x3 P.>>= \v3 ->
    liftE (P.return (P.enumFromTo v1 v3))

  enumFromThenTo = rtrnFunc $ \x1 -> rtrnFunc $ \x2 -> rtrnFunc $ \x3 ->
    x1 P.>>= \v1 -> x2 P.>>= \v2 -> x3 P.>>= \v3 ->
    liftE (P.return (P.enumFromThenTo v1 v2 v3))

instance EnumND Integer where
  succ = app (+) (P.return 1)
  pred = app (-) (P.return 1)

  toEnum   = toInteger
  fromEnum = fromInteger

  enumFrom = rtrnFunc $ \x1 ->
    x1 P.>>= \v1 ->
    liftE (P.return (P.enumFrom v1))

  enumFromThen = rtrnFunc $ \x1 -> rtrnFunc $ \x2 ->
    x1 P.>>= \v1 -> x2 P.>>= \v2 ->
    liftE (P.return (P.enumFromThen v1 v2))

  enumFromTo = rtrnFunc $ \x1 -> rtrnFunc $ \x3 ->
    x1 P.>>= \v1 -> x3 P.>>= \v3 ->
    liftE (P.return (P.enumFromTo v1 v3))

  enumFromThenTo = rtrnFunc $ \x1 -> rtrnFunc $ \x2 -> rtrnFunc $ \x3 ->
    x1 P.>>= \v1 -> x2 P.>>= \v2 -> x3 P.>>= \v3 ->
    liftE (P.return (P.enumFromThenTo v1 v2 v3))

-- * Lifted Bounded type class, instances and functions

-- | Lifted Bounded type class
class BoundedND a where
  minBound :: SML a
  maxBound :: SML a

instance BoundedND Int where
  minBound = P.return P.minBound
  maxBound = P.return P.maxBound

class IsStringND a where
  fromString :: SML (StringND --> a)

instance (a ~ Char) => IsStringND (ListND a) where
  fromString = rtrnFunc $ \x -> x

{-

class Fractional a => Floating a where
  pi                  :: a
  exp, log, sqrt      :: a -> a
  (**), logBase       :: a -> a -> a
  sin, cos, tan       :: a -> a
  asin, acos, atan    :: a -> a
  sinh, cosh, tanh    :: a -> a
  asinh, acosh, atanh :: a -> a

  log1p               :: a -> a
  expm1               :: a -> a
  log1pexp            :: a -> a
  log1mexp            :: a -> a

  x ** y              =  exp (log x * y)
  logBase x y         =  log y / log x
  sqrt x              =  x ** 0.5
  tan  x              =  sin  x / cos  x
  tanh x              =  sinh x / cosh x

  log1p x = log (1 + x)
  expm1 x = exp x - 1
  log1pexp x = log1p (exp x)
  log1mexp x = log1p (negate (exp x))
-}
