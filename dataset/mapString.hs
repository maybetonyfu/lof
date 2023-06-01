import Data.List

charToMaybe :: [Maybe Char]
charToMaybe =  map id ['a', 'b', 'c']


-- theme: list, builtin
-- goanna results: 4
-- oracle: true
-- intended fix: 2
-- response time: 0.22016501426696777
-- mus size: 5
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 3
-- goanna2: 4
-- goanna3: 5
-- goanna4: 6
-- goanna5: 6