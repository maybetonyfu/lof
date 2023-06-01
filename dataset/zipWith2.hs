
makeTuple x y = (y, x)

f :: [a] -> [b] -> [(a, b)]
f xs ys = zipWith makeTuple xs ys

-- theme: list, function, tuple, builtin
-- goanna results: 15
-- oracle: true
-- intended fix: 3
-- response time: 0.330035924911499
-- mus size: 8
-- ghc loc: 1
-- ghc diagnosis: 1
-- ghc fix: 0
-- goanna1: 2
-- goanna2: 3
-- goanna3: 4
-- goanna4: 4
-- goanna5: 5