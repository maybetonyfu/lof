sumEven :: [Int] -> Int
sumEven xs = sum (filter even xs)

stringInts = ["1", "2", "3", "4"]

sumEvenStringInts :: Int
sumEvenStringInts = sumEven (map id stringInts)

-- theme: list
-- goanna results: 7
-- oracle: true
-- intended fix: 3