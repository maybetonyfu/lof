left (Left a) = Just a
left (Right _) = Nothing

eitherToMaybe :: Either Int String -> Int
eitherToMaybe e = left e

-- theme: adt
-- goanna results: 3
-- oracle: true
-- intended fix: 3
-- response time: 0.22824597358703613
-- mus size: 4
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0

-- goanna1: 1
-- goanna2: 3
-- goanna3: 4
-- goanna4: 4
-- goanna5: 4