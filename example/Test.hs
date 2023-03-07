sum' [] = []
sum' (x:xs) = x + sum' xs

f u =
  case u of
   3 -> 3
   4 -> '4'
   '5' -> 2