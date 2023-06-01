doubleSelf2 :: (Functor f) => f a -> f a
doubleSelf2 xs = fmap (\a -> mappend a a) xs

-- theme:  function, type-class, builtin
-- goanna results: 3
-- oracle: true
-- intended fix: 3
-- response time: 0.18451809883117676
-- mus size: 4
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 2
-- goanna3: 3
-- goanna4: 3
-- goanna5: 3