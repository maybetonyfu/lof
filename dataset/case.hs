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