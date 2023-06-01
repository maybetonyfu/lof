devideAsMaybe x y =
  case y == 0.0 of
    True -> Nothing
    False -> x / y

-- theme: basics
-- goanna results: 2
-- oracle: true
-- intended fix: 2
-- response time: 0.15875792503356934
-- mus size: 2
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 2
-- goanna3: 2
-- goanna4: 2
-- goanna5: 2