class A a where
    f :: a -> a

instance A Int

instance A Float

class (A a) => B a where
    g :: a -> a

instance B Int

x = g 3