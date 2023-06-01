data BinTree = Leaf | Label Int BinTree BinTree

depth :: BinTree -> Int
depth Leaf = 0
depth (Leaf a l r) = 1 + max (depth l) (depth r)


-- theme: adt, pattern-matching, builtin
-- goanna results: 1
-- oracle: true
-- intended fix: 1
-- response time: 0.16170787811279297
-- mus size: 1
-- ghc loc: 1
-- ghc diagnosis: 1
-- ghc fix: 0

-- goanna1: 1
-- goanna2: 1
-- goanna3: 1
-- goanna4: 1
-- goanna5: 1