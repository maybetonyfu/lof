antisymatric :: Eq a => a -> a -> Bool
antisymatric x y = x >= y && y >= x

-- theme: type-class
-- goanna results: 1
-- oracle: true
-- intended fix: 1
-- response time: 0.3110361099243164
-- mus size: 7
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0

-- goanna1: 2
-- goanna2: 4
-- goanna3: 6
-- goanna4: 6
-- goanna5: 7