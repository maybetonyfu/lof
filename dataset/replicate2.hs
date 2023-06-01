import Data.List

repeatChar :: Char -> Int -> [String]
repeatChar c n = replicate c n


-- theme: function, list
-- goanna results: 4
-- oracle: true
-- intended fix: 4
-- response time: 0.14278602600097656
-- mus size: 5
-- ghc loc: 1
-- ghc diagnosis: 1
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 2
-- goanna3: 3
-- goanna4: 3
-- goanna5: 3