
makeTuple x y = (y, x)

f :: [a] -> [b] -> [(a, b)]
f xs ys = zipWith makeTuple xs ys

-- theme: list, function, tuple
-- goanna results: 15
-- oracle: true
-- intended fix: 3