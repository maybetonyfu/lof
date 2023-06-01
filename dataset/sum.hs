getsum [] = 0
getsum [x] = x
getsum (x:xs) = x + getsum xs

check (x:xs)
  | x `mod` 3 == 0 || x `mod` 5 == 0 = x + check xs
  | otherwise = check xs

y :: [Int]
y = check [1..999]

-- theme: list
-- goanna results: 3
-- oracle: true
-- intended fix: 1
-- response time: 1.5119547843933105
-- mus size: 5
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 2
-- goanna3: 3
-- goanna4: 3
-- goanna5: 3