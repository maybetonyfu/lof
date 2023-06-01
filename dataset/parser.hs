import Control.Monad.State

getToken :: State [Char] Char
getToken = do
    tokens <- get
    put (tail tokens)
    return (head tokens)

parseDigitMaybe :: State [Char] Int
parseDigitMaybe = do
  token <- getToken
  return (if token `elem` "0123456789" then Just (read [token]) else Nothing)

-- theme: monad, builtin
-- goanna results: 3
-- oracle: true
-- intended fix: 3
-- response time: 0.4352858066558838
-- mus size: 5
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 3
-- goanna3: 4
-- goanna4: 4
-- goanna5: 4