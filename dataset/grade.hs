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