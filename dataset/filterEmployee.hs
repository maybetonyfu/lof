data Person = Employee String Int | Customer String Int

filterEmployees xs = filter isEmployee xs
  where isEmployee (Employee _ _) = True
        isEmployee _ = False

people :: [Person]
people = [Employee "Alice" 123, Customer "Bob" 456, Employee "Charlie" 789]

employees :: [Person]
employees = filter filterEmployees people


-- theme: list
-- goanna results: 3
-- oracle: true
-- intended fix: 2
-- response time: 0.3042910099029541
-- mus size: 6
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0

-- goanna1: 1
-- goanna2: 2
-- goanna3: 3
-- goanna4: 3
-- goanna5: 3
