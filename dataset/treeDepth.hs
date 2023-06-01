
data Tree a = Empty | Branch a (Tree a) (Tree a)
leaf x = Branch x Empty Empty

countBranches Empty = 0
countBranches (Branch _ l r) = 1 + l + r

 -- theme: adt
-- goanna results: 9
-- oracle:  true
-- intended fix: 7
-- response time: 0.43355584144592285
-- mus size: 7
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 2
-- goanna2: 4
-- goanna3: 5
-- goanna4: 6
-- goanna5: 6