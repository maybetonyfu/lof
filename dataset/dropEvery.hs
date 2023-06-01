divides x y = y `mod` x == 0


dropEvery [] _ = []
dropEvery (x:xs) n = dropEvery' (x:xs) n 1

dropEvery' :: [Int] -> Int -> Int -> [Int]
dropEvery' [] _ _ = []
dropEvery' (x:xs) n i =
    let current =
            if n `divides` i
                then []
                else [x]
    in current : dropEvery' xs n (i+1)


-- theme: list
-- goanna results: 4
-- oracle: true
-- intended fix: 1
-- response time: 0.7817790508270264
-- mus size: 5
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 2
-- goanna2: 3
-- goanna3: 4
-- goanna4: 5
-- goanna5: 5
