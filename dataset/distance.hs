distence x y = (x + y) * (x - y)

distances xs ys = zipWith distence xs ys

sumDistances :: Int
sumDistances = distances [1 ,3] [2, 4]

-- theme: list, builtin
-- goanna results: 3
-- oracle: true
-- intended fix: 2
-- response time: 0.3244650363922119
-- mus size: 3
-- ghc loc: 1
-- ghc diagnosis: 0
-- ghc fix: 0

-- goanna1: 1
-- goanna2: 2
-- goanna3: 3
-- goanna4: 3
-- goanna5: 3