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