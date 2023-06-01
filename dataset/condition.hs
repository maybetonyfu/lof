data U = UCon Bool Int (Int, Int)
u :: U -> Int
u (UCon x y j) =
  if x
    then j
    else fst y + snd y

-- theme: adt
-- goanna results: 15
-- oracle: true
-- intended fix: 1
-- response time: 0.6489279270172119
-- mus size: 10
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 2
-- goanna2: 3
-- goanna3: 5
-- goanna4: 6
-- goanna5: 7