data Category = Untaxed Int | Taxed  Int Float

getSalary (Taxed salary) = salary
getSalary (Taxed salary tax) = salary - (floor tax)

-- theme:  adt, basics, builtin
-- goanna results: 3
-- oracle: true
-- intended fix: 1
-- response time: 0.17459392547607422
-- mus size: 4
-- ghc loc: 1
-- ghc diagnosis: 1
-- ghc fix: 0

-- goanna1: 1
-- goanna2: 2
-- goanna3: 2
-- goanna4: 2
-- goanna5: 2