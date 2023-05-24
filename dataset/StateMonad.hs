import Control.Monad.State

x :: State Int Int
x = do
  put '3'
  v <- get
  return v

-- theme: monad
-- goanna results: 3
-- oracle: true
-- intended fix: 1