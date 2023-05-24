data Category = Untaxed Int | Taxed  Int Float

getSalary (Taxed salary) = salary
getSalary (Taxed salary tax) = salary - (floor tax)

-- theme:  adt, basics
-- goanna results: 3
-- oracle: true
-- intended fix: 1