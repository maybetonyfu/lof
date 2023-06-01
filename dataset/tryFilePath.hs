findFirstExe :: [String] -> IO (Maybe String, [String])
findFirstExe = findFirstExe' []

findFirstExe' fs' []     = return ("", reverse fs')
findFirstExe' fs' (f:fs) = do
  isExe <- doesExecutableExist f
  if isExe
    then return (f, reverse fs')
    else findFirstExe' (f:fs') fs

doesExecutableExist :: String -> IO Bool
doesExecutableExist _ = do
  return True

-- theme: monad
-- goanna results: 4
-- oracle: true
-- intended fix: 1
-- response time: 0.5084478855133057
-- mus size: 8
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 2
-- goanna3: 3
-- goanna4: 4
-- goanna5: 4
