myelem a [] = False
myelem a (x:xs) = if a == x then True else myelem xs

myNotElem :: Eq a => a -> [a] -> Bool
myNotElem a xs = not (myelem a)

-- theme: list
-- goanna results: 2
-- oracle: true
-- intended fix: 1