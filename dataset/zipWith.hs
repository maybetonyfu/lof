
f :: a -> b -> (a, b)
f x y = (x, y)

g :: [a] -> [b] -> [(a, b)]
g xs ys = zipWith f xs xs

-- theme: list, function, tuple, builtin
-- goanna results: 9
-- oracle: true
-- intended fix: 2
-- response time: 0.316586971282959
-- mus size: 7
-- ghc loc: 1
-- ghc diagnosis: 1
-- ghc fix: 0

-- goanna1: 1
-- goanna2: 2
-- goanna3: 3
-- goanna4: 5
-- goanna5: 6