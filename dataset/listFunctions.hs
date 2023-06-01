last' :: [a] -> a
last' [x] = x
last' (x:xs) = xs

-- Takes the first n elements from a list
take' :: Int -> [a] -> [a] 
take' n [] = []
take' n (x:xs) = x : (take' (n - 1) xs)

-- theme: list
-- goanna results: 4
-- oracle: true
-- intended fix: 1
-- response time: 0.39415597915649414
-- mus size: 7
-- ghc loc: 1
-- ghc diagnosis: 0
-- ghc fix: 0

-- goanna1: 1
-- goanna2: 2
-- goanna3: 4
-- goanna4: 5
-- goanna5: 5