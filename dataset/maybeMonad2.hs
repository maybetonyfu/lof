monadAdd :: Int -> Int -> Maybe Int
monadAdd x y= do
  a <- Just (x + 3)
  b <- Just (y + 4)
  if a + b > 10
    then a
    else b

-- theme: monad
-- goanna results: 11
-- oracle: true
-- intended fix: 1
