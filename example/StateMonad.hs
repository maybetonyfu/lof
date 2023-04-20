import Control.Monad.State

x :: State Int Int
x = do
  put '3'
  v <- get
  return v