import Control.Monad.State

data Stack = Stack [Int]

pop :: State Stack Int
pop = do
  Stack xs <- get
  pop' xs

pop' [] = undefined
pop' (x:xs) = do
  put (Stack xs)
  return xs

push :: Int -> State Stack ()
push x = do
  Stack xs <- get
  put (Stack (x : xs))


-- theme: monad
-- goanna results: 6
-- oracle: true
-- intended fix: 1
-- response time: 0.6177709102630615
-- mus size: 8
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0

-- goanna1: 1
-- goanna2: 2
-- goanna3: 3
-- goanna4: 5
-- goanna5: 6