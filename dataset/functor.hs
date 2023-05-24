
doubleSelf :: (Functor f) => f a -> f a
doubleSelf xs = xs `mappend` xs

-- theme:  function, type-class
-- goanna results: 2
-- oracle: true
-- intended fix: 2