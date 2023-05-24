sumEven :: [Int] -> Int
sumEven xs = sum (filter even xs)

integers = filter read ["1", "2", "3", "4"]

sumEvenStringInts :: Int
sumEvenStringInts = sumEven integers

-- theme: list
-- goanna results: 8
-- oracle: true
-- intended fix: 1