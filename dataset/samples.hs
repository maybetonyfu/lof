
samples = ["2.0", 3.0, 4.0, 5.0, 6.0]

range xs = fromIntegral (length xs)

mean xs = sum xs / range xs

sd xs = sqrt (variance xs)

variance xs = map (\x -> (x - mean xs) ^ 2 / range xs) xs

sampleSd = sd samples

-- theme: basics
-- goanna results: 2
-- oracle: true
-- intended fix: 2
