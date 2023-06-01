data Category = Untaxed Int | Taxed  Int Float

getSalary :: Category -> Int
getSalary (Untaxed salary) = salary
getSalary (Taxed salary tax) = floor salary - tax

-- theme:  adt, basics, builtin
-- goanna results: 3
-- oracle: true
-- intended fix: 1
-- response time: 0.32073187828063965
-- mus size: 9
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0

-- goanna1: 1
-- goanna2: 2
-- goanna3: 3
-- goanna4: 6
-- goanna5: 7
