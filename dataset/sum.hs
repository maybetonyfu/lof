getsum [] = 0
getsum [x] = x
getsum (x:xs) = x + getsum xs

check (x:xs)
  | x `mod` 3 == 0 || x `mod` 5 == 0 = x + check xs
  | otherwise = check xs
--
y :: [Int]
y = check [1..999]

-- theme: list
-- goanna results: 3
-- oracle: true
-- intended fix: 1
