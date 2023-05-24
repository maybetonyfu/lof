
f :: a -> b -> (a, b)
f x y = (x, y)

g :: [a] -> [b] -> [(a, b)]
g xs ys = zipWith f xs xs

-- theme: list, function, tuple
-- goanna results: 9
-- oracle: true
-- intended fix: 2