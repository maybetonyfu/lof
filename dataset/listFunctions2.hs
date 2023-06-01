myelem a [] = False
myelem a (x:xs) = if a == x then True else myelem xs

myNotElem :: Eq a => a -> [a] -> Bool
myNotElem a xs = not (myelem a)

-- theme: list, builtin
-- goanna results: 2
-- oracle: true
-- intended fix: 1
-- response time: 0.3161468505859375
-- mus size: 6
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 2
-- goanna3: 2
-- goanna4: 2
-- goanna5: 2