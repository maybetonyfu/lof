data Category = Untaxed Int | Taxed  Int Float

getSalary :: Category -> Int
getSalary (Untaxed salary) = salary
getSalary (Taxed salary tax) = floor salary - tax

-- theme:  adt, basics
-- goanna results: 3
-- oracle: true
-- intended fix: 1