class MyEq a where
  myeq :: a -> a -> Bool

instance MyEq Int where
  myeq x y = x == y

instance MyEq Bool where
  myeq True True = True
  myeq False False = True
  myeq _ _ = False

data Pair a b = Pair a b

instance (MyEq a, MyEq b) => MyEq (Pair a b)

data MyList a = Nil | Cons a (MyList a)

instance MyEq a => MyEq (MyList a)

isEq = myeq True False

-- theme: type class
-- goanna results: 3
-- oracle: true
-- intended fix: 2