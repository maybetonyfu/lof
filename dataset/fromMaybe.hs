fromMaybe' m x =
    case m of
        Nothing -> x
        Just a -> a

maybeOrZero m = fromMaybe' 0 m

-- add two maybe values
a = maybeOrZero (Just 3)
b = maybeOrZero Nothing
c = a + b

-- theme:  adt
-- goanna results: 31
-- oracle: true
-- intended fix: 3
-- response time: 1.3580849170684814
-- mus size: 14
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0

-- goanna1: 2
-- goanna2: 3
-- goanna3: 4
-- goanna4: 5
-- goanna5: 7