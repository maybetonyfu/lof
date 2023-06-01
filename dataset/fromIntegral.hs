f xs y = map (+ y) xs

xs :: [Int]
xs = [1.0, 2.0, 3.0, 4.0, 5.0]


ys :: [Float]
ys = f xs 2.5

-- theme:  basics, builtin
-- goanna results: 10
-- oracle: true
-- intended fix: 1
-- response time: 0.8921291828155518
-- mus size: 10
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 7
-- goanna3: 8
-- goanna4: 9
-- goanna5: 10