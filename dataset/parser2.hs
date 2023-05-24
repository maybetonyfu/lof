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

-- theme: monad
-- goanna results: 3
-- oracle: true
-- intended fix: 1