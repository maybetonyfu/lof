seconds :: [(a,b)] -> [b]
seconds xs = map fst xs

-- theme: tuple
-- goanna results: 5
-- oracle: true
-- intended fix: 1