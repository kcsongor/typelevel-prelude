{-# LANGUAGE AllowAmbiguousTypes #-}

module Type.Prelude
  ( Fst, Snd
  , Id, Const, Flip, type (.), type ($), S
  , CatMaybes, Elem
  , Ord(..), Semigroup(..), Monoid(..)
  , Functor(..), type (<$>), Applicative(..), Monad(..) , type (>>)
  , Alternative(..)
  , Return
  , Map, type (++), Foldl, Foldl1, FoldMap, All, Everywhere
  , Gmap, GmapMaybe, MCons, Listify
  , Lens
  , Constant(..), UnConstant
  , View
  , Identity(..), RunIdentity
  , Update
  , Vector(..)
  , HListF(..)
  , HList
  , Bool(..)
  , Maybe(..)
  , If
  , type (==)
  ) where

import Data.Type.Bool
import Data.Type.Equality
import Data.Ord (Ordering(..))
import Data.Kind (Type, Constraint)
import Prelude (Bool(..))
import Data.Maybe
import GHC.TypeLits

--------------------------------------------------------------------------------
-- * Standard

type family Fst (p :: (a, b)) :: a where
  Fst '(x, _) = x

type family Snd (p :: (a, b)) :: b where
  Snd '(_, y) = y

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

-- | S combinator
type family S (f :: r ->{m} a ->{m} b) (x :: r ->{m} a) (s :: r) where
  S f x r = f r (x r)

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

class Ord a where
  type Compare (x :: a) (y :: a) :: Ordering

instance Ord Symbol where
  type Compare x y = CmpSymbol x y

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

instance Functor ((,) a) where
  type Fmap f '(x, y) = '(x, f y)

instance Functor Maybe where
  type Fmap f 'Nothing = 'Nothing
  type Fmap f ('Just x) = 'Just (f x)

class Applicative f where
  type (<*>) (fab :: f (a ->{n} b)) (fa :: f a) :: f b
  type (<*>) fab fa = fab >>= Flip (<$>) fa
  type Pure (v :: a) :: f a
infixl 4 <*>

instance Applicative Maybe where
  type Pure x = 'Just x
  type 'Nothing <*> _ = 'Nothing
  type 'Just _ <*> 'Nothing = 'Nothing
  type 'Just f <*> 'Just x = 'Just (f x)

instance Monoid a => Applicative ((,) a) where
  type instance Pure x = '(Mempty, x)
  type instance '(s1, f) <*> '(s2, x) = '(s1 <> s2, f x)

class Applicative m => Alternative m where
  type family Empty :: m a
  type family (<|>) (x1 :: m a) (x2 :: m a) :: m a

instance Alternative Maybe where
  type instance Empty = 'Nothing
  type instance 'Nothing <|> 'Nothing = 'Nothing
  type instance ('Just x) <|> _ = 'Just x
  type instance 'Nothing <|> 'Just x = 'Just x

class Applicative m => Monad (m :: Type -> Type) where
  type (>>=) (ma :: m a) (amb :: a ->{n} m b) :: m b

type Return = Pure

instance Monad Maybe where
  type 'Nothing >>= _ = 'Nothing
  type 'Just x >>= f = f x

type (>>) ma f = ma >>= (Const f)

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

type family RunIdentity (c :: Identity a) :: a where
  RunIdentity ('MkIdentity a) = a

instance Functor Identity where
  type Fmap f ('MkIdentity a) = 'MkIdentity (f a)

type family Update l f st where
  Update lens f s = RunIdentity (lens $ MkIdentity . f) s

-- data Person = MkPerson Symbol Nat Address
-- data Address = MkAddress Symbol Nat
-- 
-- type Joe = 'MkPerson "Joe" 32 ('MkAddress "Elm Street" 13)
-- 
-- type family AddressL f t where
--   AddressL f ('MkPerson name age address) = 'MkPerson name age <$> f address
-- 
-- type family HouseL f t where
--   HouseL f ('MkAddress street house) = 'MkAddress street <$> f house

data Vector k n where
  VNil :: Vector k 0
  (:>) :: k -> Vector k n -> Vector k (n + 1)
infixr 5 :>

data HListF (f :: k ->{m} Type) (xs :: Vector k n) where
  HNil ::                       HListF f  'VNil
  (:&) :: f x -> HListF f xs -> HListF f (x ':> xs)

infixr 5 :&

type HList = HListF Id
