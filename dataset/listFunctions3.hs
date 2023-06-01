last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs

-- Takes the first n elements from a list
take' :: Int -> [a] -> [a]
take' n [] = []
take' n (x:xs) = x ++ (take' (n - 1) x)

-- theme: list
-- goanna results: 7
-- oracle: true
-- intended fix: 2
-- response time: 0.513375997543335
-- mus size: 7
-- ghc loc: 1
-- ghc diagnosis: 0
-- ghc fix: 0

-- goanna1: 1
-- goanna2: 3
-- goanna3: 4
-- goanna4: 5
-- goanna5: 5