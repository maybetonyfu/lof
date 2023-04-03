module Prelude where

undefined = error

length :: [a] -> Int
length = undefined

id :: a -> a
id = undefined

(+) :: Int -> Int -> Int
(+) = undefined

(-) :: Int -> Int -> Int
(-) = undefined

(*) :: Int -> Int -> Int
(*) = undefined

data Bool = True | False

data Maybe a = Just a | Nothing

class Functor f where
    fmap :: (a -> b) -> f a -> f b

class Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

instance Monad Maybe