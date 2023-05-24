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