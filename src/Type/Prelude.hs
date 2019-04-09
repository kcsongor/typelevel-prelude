{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnsaturatedTypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Type.Prelude where

import Data.Kind (Type, Constraint)

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

--------------------------------------------------------------------------------
-- * Lists
type family Map (f :: a ->{m} b) (as :: [a]) :: [b] where
  Map _ '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

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
