sumEven :: [Int] -> Int
sumEven xs = sum (filter even xs)

stringInts = ["1", "2", "3", "4"]

stringToInt = read

sumEvenStringInts :: Int
sumEvenStringInts = sumEven (map id stringInts)

-- theme: list
-- goanna results: 7
-- oracle: true
-- intended fix: 3
-- response time: 1.2379000186920166
-- mus size: 12
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 4
-- goanna2: 5
-- goanna3: 6
-- goanna4: 7
-- goanna5: 8