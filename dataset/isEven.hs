isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

containsEven :: [Int] -> Bool
containsEven xs = any (map isEven xs)

x = containsEven ["36", "8", "123"]

-- theme: list, basics, builtin
-- goanna results: 7
-- oracle: true
-- intended fix: 1
-- response time: 1.1314780712127686
-- mus size: 11
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 3
-- goanna2: 4
-- goanna3: 6
-- goanna4: 7
-- goanna5: 8