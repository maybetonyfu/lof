isEven 0 = "True"
isEven n = isOdd (n - 1)

isOdd 0 = False
isOdd n = isEven (n - 1)

-- theme: function, pattern-matching
-- goanna results: 3
-- oracle: true
-- intended fix: 1