class A a where
    f :: a -> a

instance A Int where
    f x = x + 3

instance A Float where
    f x = x + 3

class (A a) => B a where
    g :: a -> a

instance B Int where

x = g 3