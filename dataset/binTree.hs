data BinTree = Leaf | Label Int BinTree BinTree

depth :: BinTree -> Int
depth Leaf = 0
depth (Leaf a l r) = 1 + max (depth l) (depth r)


-- theme: adt, pattern-matching
-- goanna results: 1
-- oracle: true
-- intended fix: 1