sumEven :: [Int] -> Int
sumEven xs = sum (filter even xs)

integers = filter read ["1", "2", "3", "4"]

sumEvenStringInts :: Int
sumEvenStringInts = sumEven integers

-- theme: list, builtin
-- goanna results: 8
-- oracle: true
-- intended fix: 1
-- response time: 1.6076180934906006
-- mus size: 14
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 6
-- goanna3: 7
-- goanna4: 8
-- goanna5: 10