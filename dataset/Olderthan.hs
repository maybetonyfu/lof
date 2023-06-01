data Person = Person String Int

class HasAge a where
  ageOf :: a -> Int

instance HasAge Person where
  ageOf (Person _ age) = age

olderThan ::  a -> a -> Bool
olderThan x y = ageOf x >= ageOf y

-- theme: type-class
-- goanna results: 6
-- oracle: true
-- intended fix: 1
-- response time: 0.2529141902923584
-- mus size: 6
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0

-- goanna1: 2
-- goanna2: 3
-- goanna3: 4
-- goanna4: 5
-- goanna5: 5