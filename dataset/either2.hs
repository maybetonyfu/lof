left (Left a) = Just a
left (Right _) = Nothing

eitherToInt :: Either Int String -> Int
eitherToInt e = left e

-- theme: adt
-- goanna results: 3
-- oracle: true
-- intended fix: 2