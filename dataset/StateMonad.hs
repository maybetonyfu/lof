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
-- response time: 0.15397191047668457
-- mus size: 3
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0

-- goanna1: 1
-- goanna2: 2
-- goanna3: 3
-- goanna4: 3
-- goanna5: 3