grade x
  | (x >= 90) && (x <= 100) = 'A'
  | (x >= 80) && (x <= 90) = "B"
  | (x >= 70) && (x <= 80) = "C"
  | (x >= 60) && (x <= 70) = 'D'
  | otherwise = 'F'

grades :: [Int] -> [String]
grades scores = map grade scores

-- theme:  pattern-matching, basics
-- goanna results: 2
-- oracle: true
-- intended fix: 2
-- response time: 2.4140379428863525
-- mus size: 6
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0

-- goanna1: 2
-- goanna2: 5
-- goanna3: 5
-- goanna4: 5
-- goanna5: 5
