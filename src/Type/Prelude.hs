{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnsaturatedTypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Type.Prelude where

import Data.Kind (Type, Constraint)
import Data.Maybe

--------------------------------------------------------------------------------
-- * Standard

type family Id (a :: k) :: k where
  Id a = a

type family Const (a :: k) (b :: j) :: k where
  Const a b = a

type family Flip (f :: j ->{m} k ->{n} l) (a :: k) (b :: j)  :: l where
  Flip f a b = f b a

type family (.) (f :: b ->{m} c) (g :: a ->{n} b) (x :: a) :: c where
  (f . g) x = f (g x)

infixr 9 .

type family ($) (f :: a ->{m} b)  (x :: a) :: b where
  f $ x = f x

infixr 0 $

--------------------------------------------------------------------------------
-- * Classes

class Semigroup (s :: Type) where
  type (<>) (a :: s) (b :: s) :: s

infixr 6 <>

class Semigroup m => Monoid (m :: Type) where
  type Mempty :: m

instance Semigroup Constraint where
  type (<>) @Constraint a b = (a, b)

instance Monoid Constraint where
  type Mempty @Constraint = ()

class Functor f where
  type Fmap (fun :: a ->{m} b) (fa :: f a) :: f b

-- | Infix version of 'Fmap'
--
-- >>> :kind! ((+) 1) <$> '[1,2,3]
-- '[1,2,3]
--
type (<$>) = (Fmap :: (a ->{m} b) ~> f a ~> f b)
infixl 4 <$>

instance Functor [] where
  type Fmap f xs = Map f xs

--------------------------------------------------------------------------------
-- * Lists
type family Map (f :: a ->{m} b) (as :: [a]) :: [b] where
  Map _ '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

type family (++) (as :: [k]) (bs :: [k]) :: [k] where
  '[] ++ ys       = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

infixr 5 ++

type family Foldl (f :: b ->{m} a ->{m} b) (z :: b) (xs :: [a]) :: b where
  Foldl f z '[] = z
  Foldl f z (x ': xs) = Foldl f (f z x) xs

--------------------------------------------------------------------------------  
-- * Misc

type family FoldMap (f :: a ->{m} b) (as :: [a]) :: b where
  FoldMap _ '[]       = Mempty
  FoldMap f (x ': xs) = f x <> FoldMap f xs

type family All :: (k ->{m} Constraint) ~> [k] ~> Constraint where
  All = FoldMap

--------------------------------------------------------------------------------
-- * Scrap your boilerplate

-- | Apply a function to all matching subterms of some structure.
--
-- For example, we can add 10 to every Nat in a list
--
-- >>> :kind! Everywhere ((+) 10) '[ 'Just '(1,2), 'Just '(3,4) ]
-- >>> '[ 'Just '(11, 12), 'Just '(13, 14)]
--
type family Everywhere (f :: b ->{m} b) (st :: a) :: a where
  Everywhere f st = f st
  Everywhere f (st x) = Everywhere f st (Everywhere f x)
  Everywhere f st = st

-- | Collect all subterms 
type family Gmap (f :: b ->{m} r) (st :: a) :: [r] where
  Gmap f (st x) = f (st x) ': Gmap f st ++ Gmap f x
  Gmap f (st x) = Gmap f st ++ Gmap f x
  Gmap f st   = '[f st]
  Gmap f st   = '[]

-- | Return all the types of kind 'k' from some structure.
--
-- >>> :kind! Listify Nat '[ 'Just '(1,2), 'Nothing]
-- '[1, 2]
--
-- If the kind in question is recursive, then all self-similar subterms are returned.
--
-- >>> :kind! Listify [Nat] '[1,2,3]
-- '[ '[1, 2, 3], '[2, 3], '[3], '[]]
type family Listify k (st :: a) :: [k] where
  Listify k st = Gmap (Id @k) st
