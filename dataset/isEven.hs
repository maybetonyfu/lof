isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

containsEven :: [Int] -> Bool
containsEven xs = any (map isEven xs)

x = containsEven ["36", "8", "123"]

-- theme: list, basics
-- goanna results: 7
-- oracle: true
-- intended fix: 1