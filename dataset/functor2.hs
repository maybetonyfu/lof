doubleSelf2 :: (Functor f) => f a -> f a
doubleSelf2 xs = fmap (\a -> mappend a a) xs

-- theme:  function, type-class
-- goanna results: 3
-- oracle: true
-- intended fix: 3