data BinTree = Leaf | Label Int BinTree BinTree

depth :: BinTree -> Int
depth Leaf = 0
depth (Label a l r) = 1 + max l r


-- theme: adt, pattern-matching
-- goanna results: 5
-- oracle: true
-- intended fix: 4