parsingStringFlat flag =
  case flag of
        "True" -> 1
        False -> 2
        "True1" -> 3
        "False2" -> 4
        _ -> 0

-- theme: basics
-- goanna results: 2
-- oracle: true
-- intended fix: 1
-- response time: 0.19045686721801758
-- mus size: 4
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0

-- goanna1: 1
-- goanna2: 4
-- goanna3: 4
-- goanna4: 4
-- goanna5: 4