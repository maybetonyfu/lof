samples = [3.0, 4.0, 5.0, 6.0]

range xs = length xs

mean xs = sum xs / range xs

sd xs = sqrt (variance xs)

variance xs = sum (map (\x -> (x - mean xs) ^ 2 / range xs) xs)


-- theme: function
-- goanna results: 5
-- oracle: true
-- intended fix: 1
-- response time: 1.9231469631195068
-- mus size: 11
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0

-- goanna1: 1
-- goanna2: 3
-- goanna3: 4
-- goanna4: 5
-- goanna5: 5