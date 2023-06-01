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
-- response time: 0.37055015563964844
-- mus size: 3
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 2
-- goanna3: 3
-- goanna4: 3
-- goanna5: 3