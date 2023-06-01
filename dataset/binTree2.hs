data BinTree = Leaf | Label Int BinTree BinTree

depth :: BinTree -> Int
depth Leaf = 0
depth (Label a l r) = 1 + max l r


-- theme: adt, pattern-matching
-- goanna results: 5
-- oracle: true
-- intended fix: 4
-- response time: 0.27956724166870117
-- mus size: 6
-- ghc loc: 1
-- ghc diagnosis: 1
-- ghc fix: 0

-- goanna1: 1
-- goanna2: 3
-- goanna3: 4
-- goanna4: 5
-- goanna5: 5