incr n = n + 1
decr n = n - 1

incrementOrDecrement True c = incr c
incrementOrDecrement a x =  decr a

-- theme: basics
-- goanna results: 7
-- oracle: true
-- intended fix: 2
-- response time: 0.23059797286987305
-- mus size: 7
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 2
-- goanna3: 3
-- goanna4: 4
-- goanna5: 5