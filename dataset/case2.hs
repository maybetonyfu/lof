devideAsMaybe x y =
  case y == 0.0 of
    True -> Nothing
    False -> x / y

-- theme: basics
-- goanna results: 2
-- oracle: true
-- intended fix: 2