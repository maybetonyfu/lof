import Control.Monad.State

sumState :: Int -> State Int Int
sumState n = do
  modify (+n)
  get

multiplyState :: Int -> State Int Int
multiplyState n = do
  x <- get
  put (x * n)


-- theme: monad
-- goanna results: 2
-- oracle: true
-- intended fix: 2
-- response time: 0.19339585304260254
-- mus size: 5
-- ghc loc: 1
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 2
-- goanna3: 2
-- goanna4: 2
-- goanna5: 2