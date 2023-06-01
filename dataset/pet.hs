
data Pet = Cat String | Dog String | Snake

petIsDog :: Pet -> Bool
petIsDog Dog = True
petIsDog _ = False

-- theme: adt, pattern-matching
-- goanna results: 2
-- oracle: true
-- intended fix: 1
-- response time: 0.1154630184173584
-- mus size: 4
-- ghc loc: 1
-- ghc diagnosis: 1
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 2
-- goanna3: 2
-- goanna4: 2
-- goanna5: 2