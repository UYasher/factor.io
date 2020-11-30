{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module State (State, get, put, modify, runState, evalState, execState) where

import Control.Monad (ap, liftM)

newtype State s a = S {runState :: s -> (a, s)}

instance Monad (State s) where
  return :: a -> State s a
  return x = S (x,) -- this tuple section (x,) is equivalent to \y -> (x,y)

  (>>=) :: State s a -> (a -> State s b) -> State s b
  st >>= f =
    S
      { runState = \s ->
          let (a, s') = runState st s
           in runState (f a) s'
      }

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure = return
  (<*>) = ap

evalState :: State s a -> s -> a
evalState st s = fst (runState st s)

execState :: State s a -> s -> s
execState st s = snd (runState st s)

get :: State s s
get = S $ \s -> (s, s)

put :: s -> State s ()
put s' = S $ const ((), s')

modify :: (s -> s) -> State s ()
modify f = do
  s <- get
  put (f s)
