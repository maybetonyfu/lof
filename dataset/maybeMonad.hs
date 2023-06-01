
monadAdd :: Int -> Int -> Maybe Int
monadAdd x y= do
  a <- Just (x + 3)
  b <- Just (y + 4)
  if a + b > 10
    then return Nothing
    else return (Just (a * b))

-- theme: monad
-- goanna results: 5
-- oracle: true
-- intended fix: 1
-- response time: 0.4460740089416504
-- mus size: 5
-- ghc loc: 1
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 2
-- goanna2: 3
-- goanna3: 4
-- goanna4: 4
-- goanna5: 5