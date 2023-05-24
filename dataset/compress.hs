compress = foldr skipDups

skipDups x [] = [x]
skipDups x acc
   | x == head acc = acc
   | otherwise = x : acc

expect = [3,4,5,6]

actual :: [Int]
actual = compress [3,3,4,5,6,6]

-- theme: basics, list
-- goanna results: 3
-- oracle: true
-- intended fix: 2
