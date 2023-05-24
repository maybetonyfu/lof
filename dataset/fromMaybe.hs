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