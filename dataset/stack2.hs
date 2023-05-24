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