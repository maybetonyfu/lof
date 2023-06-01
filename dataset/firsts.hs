firsts :: [(a,b)] -> [b]
firsts xs = map fst xs


-- theme:  tuple, function, list, builtin
-- goanna results: 5
-- oracle: true
-- intended fix: 2
-- response time: 0.17671799659729004
-- mus size: 4
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 2
-- goanna3: 3
-- goanna4: 4
-- goanna5: 4