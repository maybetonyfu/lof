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