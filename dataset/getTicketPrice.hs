data Ticket = Normal Int | Discounted Int Float

getPrice :: Ticket -> Double
getPrice (Normal price) = price
getPrice (Discounted price discount) = price * discount

-- theme:  adt, basics
-- goanna results: 9
-- oracle: true
-- intended fix: 9
-- response time: 0.3890817165374756
-- mus size: 10
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0

-- goanna1: 2
-- goanna2: 3
-- goanna3: 4
-- goanna4: 5
-- goanna5: 6