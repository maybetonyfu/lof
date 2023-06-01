grade x
  | (x >= 90) && (x <= 100) = 'A'
  | (x >= 80) && (x <= 90) = 'B'
  | (x >= 70) && (x <= 80) = "C"
  | (x >= 60) && (x <= 70) = 'D'
  | otherwise = 'F'

-- theme:  adt, basics
-- goanna results: 2
-- oracle: true
-- intended fix: 1
-- response time: 0.9902048110961914
-- mus size: 4
-- ghc loc: 1
-- ghc diagnosis: 1
-- ghc fix: 0

-- goanna1: 1
-- goanna2: 5
-- goanna3: 5
-- goanna4: 5
-- goanna5: 5
