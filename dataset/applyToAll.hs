applyToAll :: [a] -> (a -> b) -> [b]
applyToAll [] _ = []
applyToAll (x:xs) f = f x : applyToAll f xs

-- theme: list
-- goanna results: 5
-- oracle: true
-- intended fix: 1
-- response time: 0.4378950595855713
-- mus size: 8
-- ghc loc: 1
-- ghc diagnosis: 0
-- ghc fix: 0

-- goanna1: 2
-- goanna2: 3
-- goanna3: 4
-- goanna4: 6
-- goanna5: 8