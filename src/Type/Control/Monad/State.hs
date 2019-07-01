module Type.Control.Monad.State
  ( State (..)
  , RunState
  , PureState
  , BindState
  , BindState2
  ) where

import Type.Prelude

newtype State s a = MkState (s ~> (s, a))

type family RunState (st :: State s a) :: (s ~> (s, a)) where
  RunState ('MkState s) = s

instance Functor (State s) where
  type Fmap f ('MkState sf) = 'MkState (Fmap f . sf)

type family PureState a s where
  PureState a s = '(s, a)

instance Applicative (State s) where
  type Pure a = 'MkState (PureState a)

type family BindState2
  (st :: (s, a))
  (k :: a ~> State s b)
  :: (s, b) where
  BindState2 '(s, x) k = RunState (k x) s

type family BindState
  (st :: s ~> (s, a))
  (f :: a ~> State s b)
  (r :: s)
  :: (s, b) where
  BindState x k s = BindState2 (x s) k

instance Monad (State s) where
  type (>>=) ('MkState ma) amb = 'MkState (BindState ma amb)
