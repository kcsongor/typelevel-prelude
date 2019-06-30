module Type.Control.Monad.Reader
  ( Reader(..)
  , RunReader
  ) where

import Type.Prelude

data Reader r a = MkReader (r ~> a)

type family RunReader (f :: Reader r a) :: r ~> a where
  RunReader ('MkReader f) = f

instance Functor (Reader r) where
  type instance Fmap f ('MkReader g) = 'MkReader (f . g)

instance Applicative (Reader r) where
  type instance Pure x = 'MkReader (Const x)
  type instance 'MkReader f <*> 'MkReader x = 'MkReader (S f x)

type family ReaderBindImpl (m :: r ~> a) (k :: a ~> Reader r b) (s :: r) where
  ReaderBindImpl m k s = RunReader (k (m s)) s

instance Monad (Reader r) where
  type instance 'MkReader m >>= k = 'MkReader (ReaderBindImpl m k)
