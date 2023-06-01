data Password = P String

-- Validate how good a password is
validate :: Password -> String
validate password = 
    if length password > 10 
        then "Great password"
        else "Password too short"

-- theme: adt, pattern-matching
-- goanna results: 4
-- oracle: true
-- intended fix: 2
-- response time: 0.1626119613647461
-- mus size: 6
-- ghc loc: 1
-- ghc diagnosis: 0
-- ghc fix: 0

-- goanna1: 1
-- goanna2: 2
-- goanna3: 3
-- goanna4: 3
-- goanna5: 3
