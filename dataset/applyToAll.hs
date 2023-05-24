applyToAll :: [a] -> (a -> b) -> [b]
applyToAll [] _ = []
applyToAll (x:xs) f = f x : applyToAll f xs

-- theme: list
-- goanna results: 5
-- oracle: true
-- intended fix: 1
