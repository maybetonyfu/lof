module Control.Monad.State where

data State s a = State (s -> (a, s))

get :: State s s
get = undefined

put :: s -> State s ()
put = undefined

modify :: (s -> s) -> State s ()
modify = undefined

instance Monad (State s)