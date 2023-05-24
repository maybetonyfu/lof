incr n = n + 1
decr n = n - 1

incrementOrDecrement True c = incr c
incrementOrDecrement a x =  decr a

-- theme: basics
-- goanna results: 7
-- oracle: true
-- intended fix: 2