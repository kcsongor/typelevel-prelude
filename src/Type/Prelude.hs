{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnsaturatedTypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module Type.Prelude where

import Data.Kind (Type, Constraint)
import Prelude (Bool(..))
import Data.Maybe
import GHC.TypeLits

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

type family CatMaybes (xs :: [Maybe k]) :: [k] where
  CatMaybes '[]             = '[]
  CatMaybes ('Just a ': as) = a ': CatMaybes as
  CatMaybes (_ ': as)       = CatMaybes as

type family Elem (x :: a) (xs :: [a]) :: Bool where
  Elem x '[] = 'False
  Elem x (x ': _) = 'True
  Elem x (_ ': xs) = Elem x xs

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

instance Semigroup Symbol where
  type (<>) a b = AppendSymbol a b

instance Monoid Symbol where
  type Mempty = ""

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

type family Foldl (f :: b ->{m} a ->{n} b) (z :: b) (xs :: [a]) :: b where
  Foldl f z '[] = z
  Foldl f z (x ': xs) = Foldl f (f z x) xs

type family Foldl1 (f :: a ->{m} a ->{n} a) (xs :: [a]) :: a where
  Foldl1 f (x ': xs) = Foldl f x xs

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
  Gmap f st = GmapMaybe ('Just . f) st

type family GmapMaybe (f :: b ->{m} Maybe r) (st :: a) :: [r] where
  GmapMaybe f (st x) = MCons (f (st x)) (GmapMaybe f st) ++ GmapMaybe f x
  GmapMaybe f (st x) = GmapMaybe f st ++ GmapMaybe f x
  GmapMaybe f st   = MCons (f st) '[]
  GmapMaybe f st   = '[]

type family MCons (x :: Maybe a) (xs :: [a]) :: [a] where
  MCons 'Nothing xs = xs
  MCons ('Just x) xs = x ': xs

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

--------------------------------------------------------------------------------
-- * Lenses

type Lens s t a b = forall m (f :: * ->{m} *). (a ~> f b) ~> (s ~> f t)

newtype Constant a b = MkConstant a

instance Functor (Constant a) where
  type Fmap _ ('MkConstant c) = 'MkConstant c

type family UnConstant (c :: Constant b a) :: b where
  UnConstant ('MkConstant a) = a

type family View (l :: (a ->{m} Constant a a) ~> (s ->{n} Constant a s)) (t :: s) :: a where
  View lens s = UnConstant (lens 'MkConstant s)

newtype Identity a = MkIdentity a

type family UnIdentity (c :: Identity a) :: a where
  UnIdentity ('MkIdentity a) = a

instance Functor Identity where
  type Fmap f ('MkIdentity a) = 'MkIdentity (f a)

type family Update l f st where
  Update lens f s = UnIdentity (lens $ MkIdentity . f) s

data Person = MkPerson Symbol Nat Address
data Address = MkAddress Symbol Nat

type Joe = 'MkPerson "Joe" 32 ('MkAddress "Elm Street" 13)

type family AddressL f t where
  AddressL f ('MkPerson name age address) = 'MkPerson name age <$> f address

type family HouseL f t where
  HouseL f ('MkAddress street house) = 'MkAddress street <$> f house

data Vector k n where
  VNil :: Vector k 0
  (:>) :: k -> Vector k n -> Vector k (n + 1)
infixr 5 :>

data HListF (f :: k ->{m} Type) (xs :: Vector k n) where
  HNil ::                       HListF f  'VNil
  (:&) :: f x -> HListF f xs -> HListF f (x ':> xs)

infixr 5 :&

type HList = HListF Id
