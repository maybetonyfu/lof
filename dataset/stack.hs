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