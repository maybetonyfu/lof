makeEven :: Int -> Int
makeEven x = if even x then True else x+1

-- theme: basics, builtin
-- goanna results: 2
-- oracle: true
-- intended fix: 1
-- response time: 0.16240191459655762
-- mus size: 3
-- ghc loc: 1
-- ghc diagnosis: 1
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 3
-- goanna3: 3
-- goanna4: 3
-- goanna5: 3