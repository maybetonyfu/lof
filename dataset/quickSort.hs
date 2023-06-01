
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
-- response time: 1.2489309310913086
-- mus size: 7
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 3
-- goanna3: 4
-- goanna4: 5
-- goanna5: 5