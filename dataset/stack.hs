import Control.Monad.State

data Stack = Stack [Int] 

pop :: State Stack Int
pop = do
  Stack xs <- get
  pop' xs

pop' [] = undefined
pop' (x:xs) = do
  put (Stack x)
  return x

push :: Int -> State Stack ()
push x = do
  Stack xs <- get
  put (Stack (x : xs))


-- theme: monad
-- goanna results: 12
-- oracle: true
-- intended fix: 1
-- response time: 0.9095690250396729
-- mus size: 11
-- ghc loc: 1
-- ghc diagnosis: 1
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 2
-- goanna3: 3
-- goanna4: 5
-- goanna5: 6