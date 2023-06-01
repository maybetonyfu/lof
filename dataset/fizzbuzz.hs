fizzBuzz :: Int -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Fizz"
           | n `mod` 3  == 0 = "Buzz"
           | otherwise       = n

-- theme: basics
-- goanna results: 11
-- oracle: true
-- intended fix: 1
-- response time: 1.6813380718231201
-- mus size: 8
-- ghc loc: 1
-- ghc diagnosis: 1
-- ghc fix: 0

-- goanna1: 1
-- goanna2: 4
-- goanna3: 8
-- goanna4: 9
-- goanna5: 10
