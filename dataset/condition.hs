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
