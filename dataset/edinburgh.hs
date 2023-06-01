test = \f -> \i -> (f i, f 2, [f,3])

-- theme: function
-- goanna results: 3
-- oracle: true
-- intended fix: 2
-- response time: 0.21521282196044922
-- mus size: 4
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 2
-- goanna3: 4
-- goanna4: 4
-- goanna5: 4
