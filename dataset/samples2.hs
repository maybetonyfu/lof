samples = [3.0, 4.0, 5.0, 6.0]

range xs = length xs

mean xs = sum xs / range xs

sd xs = sqrt (variance xs)

variance xs = map (\x -> (x - mean xs) ^ 2 / range xs) xs


-- theme: function
-- goanna results: 3/ 5
-- oracle: true
-- intended fix: 1/1