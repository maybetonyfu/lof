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

-- theme: monad
-- goanna results: 3
-- oracle: true
-- intended fix: 3