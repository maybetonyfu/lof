test = \f -> \i -> (f i, f 2, [f,3])

-- theme: function
-- goanna results: 3
-- oracle: true
-- intended fix: 2