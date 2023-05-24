data Person = Person String Int

class HasAge a where
  ageOf :: a -> Int

instance HasAge Person where
  ageOf (Person _ age) = age

olderThan ::  a -> a -> Bool
olderThan x y = ageOf x >= ageOf y

-- theme: type class
-- goanna results: 11
-- oracle: true
-- intended fix: 1
