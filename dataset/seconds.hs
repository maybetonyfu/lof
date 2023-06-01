seconds :: [(a,b)] -> [b]
seconds xs = map fst xs

-- theme: tuple
-- goanna results: 5
-- oracle: true
-- intended fix: 1
-- response time: 0.16472196578979492
-- mus size: 4
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0

-- goanna1: 1
-- goanna2: 2
-- goanna3: 3
-- goanna4: 4
-- goanna5: 4