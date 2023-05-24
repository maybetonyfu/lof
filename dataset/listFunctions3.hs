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