test :: String
test = xs : "def"
  where xs = "abc"

-- theme: list
-- goanna results: 4
-- oracle: true
-- intended fix: 2
-- response time: 0.14243793487548828
-- mus size: 4
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 2
-- goanna3: 3
-- goanna4: 4
-- goanna5: 4