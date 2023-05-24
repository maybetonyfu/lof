numberOfNumericChars :: [Char] -> Int
numberOfNumericChars xs =
    let numeric = "01234567890"
        isNumeric = filter (elem numeric) xs
    in length isNumeric

-- theme: basics
-- goanna results: 5
-- oracle: true
-- intended fix: 4