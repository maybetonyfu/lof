x :: Maybe Int
x = do
  a <- Just '3'
  b <- Just 4
  return (Just (b - 1))