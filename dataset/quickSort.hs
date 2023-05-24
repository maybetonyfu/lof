
quick :: [Int] -> [Int]
quick []   = []
quick (x:xs)=
 let littlebigs = split xs
 in
   quick (fst littlebigs)
    ++ [x]
    ++  quick (snd littlebigs)
split [] _ result = result
split (x:xs) n (littles, bigs) =
  if x < n
    then split xs n (x:littles, bigs)
    else split xs n (littles, x:bigs)

-- theme: function, list
-- goanna results: 5
-- oracle: true
-- intended fix: 1