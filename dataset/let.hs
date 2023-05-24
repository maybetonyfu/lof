numberOfNumericChars :: [Char] -> Int
numberOfNumericChars xs =
    let numeric = "01234567890"
        isNumeric = filter (`elem` numeric) xs
    in isNumeric

-- theme: basics
-- goanna results: 3
-- oracle: true
-- intended fix: 2