import Data.List

repeatChar :: Char -> Int -> Char
repeatChar c n = replicate n c


-- theme: function, list
-- goanna results: 2
-- oracle: true
-- intended fix: 2
-- response time: 0.14163827896118164
-- mus size: 5
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0

-- goanna1: 1
-- goanna2: 2
-- goanna3: 2
-- goanna4: 2
-- goanna5: 2