class MyEq a where
  myeq :: a -> a -> Bool

instance MyEq Int where
  myeq x y = x == y

instance MyEq Bool where
  myeq True True = True
  myeq False False = True
  myeq _ _ = False

data Pair a b = Pair a b

instance (MyEq a, MyEq b) => MyEq (Pair a b) where
  myeq (Pair x1 y1) (Pair x2 y2) = myeq x1 x2 && myeq y1 y2

data MyList a = Nil | Cons a (MyList a)

isEq = myeq (Pair (Cons True Nil) True) (Pair (Cons True Nil) False)

-- theme: type-class
-- goanna results: 3
-- oracle: true
-- intended fix: 2
-- response time: 0.3209669589996338
-- mus size: 6
-- ghc loc: 1
-- ghc diagnosis: 1
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 3
-- goanna3: 4
-- goanna4: 4
-- goanna5: 4