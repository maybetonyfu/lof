class A a where
    f :: a -> a

instance A Int where
    f x = x + 3

instance A Float where
    f x = x + 3

v = f True