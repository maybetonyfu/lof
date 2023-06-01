data Person = Person String Int

greet :: Person -> String
greet (Person age name) = "Hello, " ++ name ++ "! You are " ++ show age ++ " years old."


-- theme:  adt,pattern-matching,builtin
-- goanna results: 3
-- oracle: true
-- intended fix: 2
-- response time: 0.23205184936523438
-- mus size: 4
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0

-- goanna1: 1
-- goanna2: 2
-- goanna3: 3
-- goanna4: 3
-- goanna5: 3
