isEven 0 = True
isEven n = isOdd (n - 1)

isOdd 0 = "False"
isOdd n = isEven (n - 1)

-- theme: function, pattern-matching
-- goanna results: 3
-- oracle: true
-- intended fix: 2
-- response time: 0.2275240421295166
-- mus size: 4
-- ghc loc: 1
-- ghc diagnosis: 1
-- ghc fix: 0

-- goanna1: 1
-- goanna2: 2
-- goanna3: 4
-- goanna4: 4
-- goanna5: 4