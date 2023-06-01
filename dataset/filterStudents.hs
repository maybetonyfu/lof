
students = [(1, 99), (2, 60), (3, 55)]

matchFirst key (k, v) = k == key

filterById st studentId =
  filter (matchFirst studentId) st

scores = filterById students '1'

-- theme: list
-- goanna results: 12
-- oracle: true
-- intended fix: 1
-- response time: 1.0746219158172607
-- mus size: 12
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 4
-- goanna3: 5
-- goanna4: 6
-- goanna5: 7
