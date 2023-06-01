numberOfNumericChars :: [Char] -> Int
numberOfNumericChars xs =
    let numeric = "01234567890"
        isNumeric = filter (`elem` numeric) xs
    in isNumeric

-- theme: basics, builtin
-- goanna results: 3
-- oracle: true
-- intended fix: 2
-- response time: 0.16501307487487793
-- mus size: 3
-- ghc loc: 1
-- ghc diagnosis: 1
-- ghc fix: 0

-- goanna1: 1
-- goanna2: 2
-- goanna3: 3
-- goanna4: 3
-- goanna5: 3