antisymatric :: Eq a => a -> a -> Bool
antisymatric x y = x >= y && y >= x

-- theme: type class
-- goanna results: 1
-- oracle: true
-- intended fix: 1