import Data.List

rotate1 :: [a] -> [a]
rotate1 x = tail x ++ [head x]

rotate1Back :: [a] -> [a]
rotate1Back x = last x : init x
--
--
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate x 0 = x
rotate x y
  | y > 0 = rotate (rotate1 x) (y-1)
  | otherwise = rotate (rotate1Back x)


-- theme: adt, list, function
-- goanna results: 1
-- oracle: true
-- intended fix: 1
-- response time: 0.4114820957183838
-- mus size: 6
-- ghc loc: 1
-- ghc diagnosis: 1
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 1
-- goanna3: 1
-- goanna4: 1
-- goanna5: 1