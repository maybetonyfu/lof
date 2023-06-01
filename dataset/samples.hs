
samples = ["2.0", 3.0, 4.0, 5.0, 6.0]

range xs = fromIntegral (length xs)

mean xs = sum xs / range xs

variance xs = map (\x -> (x - mean xs) ^ 2 / range xs) xs


-- theme: basics, builtin
-- goanna results: 2
-- oracle: true
-- intended fix: 2
-- response time: 1.01334810256958
-- mus size: 5
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 4
-- goanna2: 5
-- goanna3: 5
-- goanna4: 5
-- goanna5: 5