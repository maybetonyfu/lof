insertAt :: Eq a => a -> [a] -> Int -> [a]
insertAt el list n =
    let accu (i, acc) x =
            if i == n
                then (acc ++ [el,x],i+1)
                else (acc ++ [x],i+1)
    in fst (foldl accu ([],1) list)

-- theme: basics
-- goanna results: 12
-- oracle: true
-- intended fix: 3
-- response time: 4.876026153564453
-- mus size: 16
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 2
-- goanna3: 4
-- goanna4: 6
-- goanna5: 7