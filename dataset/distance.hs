distence x y = (x + y) * (x - y)

distances xs ys = zipWith distence xs ys

sumDistances :: Int
sumDistances = distances [1 ,3] [2, 4]

-- theme: list
-- goanna results: 3
-- oracle: true
-- intended fix: 2
