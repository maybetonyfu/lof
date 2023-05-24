f xs y = map (+ y) xs

xs :: [Int]
xs = [1.0, 2.0, 3.0, 4.0, 5.0]


ys :: [Float]
ys = f xs 2.5

-- theme:  basics
-- goanna results: 10
-- oracle: true
-- intended fix: 1