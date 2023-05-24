
students = [(1, 99), (2, 60), (3, 55)]

matchFirst key (k, v) = k == key

filterById st studentId =
  filter (matchFirst studentId) st

scores = filterById students '1'

-- theme: list
-- goanna results: 12
-- oracle: true
-- intended fix: 1