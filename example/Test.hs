
x :: Char
x =
  let y = 3
      z u v = u
  in z y '3' 


w u = case u of
        "True" -> 3
        "False" -> 4
        True -> 1
        "False2" -> 4

z = if True then 3 else '4'