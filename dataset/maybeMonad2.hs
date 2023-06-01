monadAdd :: Int -> Int -> Maybe Int
monadAdd x y= do
  a <- Just (x + 3)
  b <- Just (y + 4)
  if a + b > 10
    then a
    else b

-- theme: monad
-- goanna results: 17
-- oracle: true
-- intended fix: 1
-- response time: 0.8862543106079102
-- mus size: 9
-- ghc loc: 1
-- ghc diagnosis: 1
-- ghc fix: 0

-- goanna1: 2
-- goanna2: 4
-- goanna3: 6
-- goanna4: 6
-- goanna5: 7