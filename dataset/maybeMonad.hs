
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
