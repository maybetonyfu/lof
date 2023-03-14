data BinTree = Leaf | Label Int BinTree BinTree

max :: Int -> Int -> Int
max = undefined

depth :: BinTree -> Int
depth Leaf = 0
depth (Leaf a l r) = 1 + max (depth l) (depth r)