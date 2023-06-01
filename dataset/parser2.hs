import Control.Monad.State

getToken :: State [Char] Char
getToken = do
    tokens <- get
    put (tail tokens)
    return (head tokens)

parseDigitInt :: State [Char] Int
parseDigitInt = do
  token <- getToken
  return (if token `elem` "0123456789" then [token] else 0)

-- theme: monad, builtin
-- goanna results: 3
-- oracle: true
-- intended fix: 1
-- response time: 0.3917508125305176
-- mus size: 9
-- ghc loc: 1
-- ghc diagnosis: 1
-- ghc fix: 0

-- goanna1: 1
-- goanna2: 3
-- goanna3: 4
-- goanna4: 4
-- goanna5: 4