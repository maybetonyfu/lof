
doubleSelf :: (Functor f) => f a -> f a
doubleSelf xs = xs `mappend` xs

-- theme:  function, type-class, builtin
-- goanna results: 2
-- oracle: true
-- intended fix: 2
-- response time: 0.1424729824066162
-- mus size: 4
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 2
-- goanna3: 2
-- goanna4: 2
-- goanna5: 2
