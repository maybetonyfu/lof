import Data.List

charToString :: [Char]
charToString =  map (replicate 10) ['a', 'b', 'c']


-- theme: list, builtin
-- goanna results: 3
-- oracle: true
-- intended fix: 2
-- response time: 0.16317200660705566
-- mus size: 5
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0

-- goanna1: 1
-- goanna2: 2
-- goanna3: 3
-- goanna4: 3
-- goanna5: 3