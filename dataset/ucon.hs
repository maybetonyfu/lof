
data Meal = Meal Bool Int (Int, Int)

mealPrice (Meal dineIn price price') =
  if dineIn
    then price'
    else fst price + snd price

actual = mealPrice (Meal False 0 (15, 10))
expect = 25

test :: Bool
test = actual == expect

-- theme: adt
-- goanna results: 12
-- oracle: true
-- intended fix: 2
-- response time: 1.4352493286132812
-- mus size: 12
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 2
-- goanna2: 3
-- goanna3: 5
-- goanna4: 7
-- goanna5: 7