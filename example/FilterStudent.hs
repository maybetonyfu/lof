students = [(1, 99), (2, 60), (3, 55)]

matchFirst key (k, v) = k == key

filterById students studentId =
  filter (matchFirst studentId) students

scores = filterById students '1'