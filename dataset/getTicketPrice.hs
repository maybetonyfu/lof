data Ticket = Normal Int | Discounted Int Float

getPrice :: Ticket -> Double
getPrice (Normal price) = price
getPrice (Discounted price discount) = price * discount

-- theme:  adt, basics
-- goanna results: 9
-- oracle: true
-- intended fix: 9