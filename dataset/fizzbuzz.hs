fizzBuzz :: Int -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Fizz"
           | n `mod` 3  == 0 = "Buzz"
           | otherwise       = n

-- theme: basics
-- goanna results: 11
-- oracle: true
-- intended fix: 1