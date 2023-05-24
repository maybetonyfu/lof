left (Left a) = Just a
left (Right _) = Nothing

eitherToMaybe :: Either Int String -> Int
eitherToMaybe e = left e

-- theme: adt
-- goanna results: 3
-- oracle: true
-- intended fix: 3