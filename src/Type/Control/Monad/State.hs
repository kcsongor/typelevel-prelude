module Type.Control.Monad.State
  ( State (..)
  , RunState
  ) where

import Type.Prelude

newtype State s a = MkState (s ~> (s, a))

type family RunState (st :: State s a) :: (s ~> (s, a)) where
  RunState ('MkState s) = s

type family PureState a s where
  PureState a s = '(s, a)

type family BindState (ma :: State s a) (amb :: a ~> State s b) (st :: s) :: (s, b) where
  BindState ('MkState ma) f s = BindState' (ma s) (RunState . f)

type family BindState' (st :: (s, a)) (f :: a ~> s ~> (s, b)) :: (s, b) where
  BindState' '(s, a) f = f a s

instance Applicative (State s) where
  type Pure a = 'MkState (PureState a)

instance Monad (State s) where
  type (>>=) ma amb = 'MkState (BindState ma amb)
