left (Left a) = Just a
left (Right _) = Nothing

eitherToInt :: Either Int String -> Int
eitherToInt e = left e

-- theme: adt
-- goanna results: 3
-- oracle: true
-- intended fix: 2
-- response time: 0.22684311866760254
-- mus size: 4
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 3
-- goanna3: 4
-- goanna4: 4
-- goanna5: 4