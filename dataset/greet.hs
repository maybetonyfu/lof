data Person = Person String Int

greet :: Person -> String
greet (Person age name) = "Hello, " ++ name ++ "! You are " ++ show age ++ " years old."


-- theme:  adt,pattern-matching
-- goanna results: 3
-- oracle: true
-- intended fix: 2