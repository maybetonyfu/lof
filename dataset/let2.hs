numberOfNumericChars :: [Char] -> Int
numberOfNumericChars xs =
    let numeric = "01234567890"
        isNumeric = filter (elem numeric) xs
    in length isNumeric

-- theme: basics, builtin
-- goanna results: 5
-- oracle: true
-- intended fix: 4
-- response time: 0.174821138381958
-- mus size: 6
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0

-- goanna1: 1
-- goanna2: 2
-- goanna3: 3
-- goanna4: 4
-- goanna5: 4