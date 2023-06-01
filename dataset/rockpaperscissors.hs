data Hand = Rock | Paper | Scissors
type Score = (Int, Int)
winsOver :: Hand -> Hand -> Bool
winsOver Rock   Scissors = True
winsOver Paper   Rock     = True
winsOver Scissors Paper    = True
winsOver _    _        = False

computeScore :: Hand -> Hand -> Score
computeScore h1 h2
  | winsOver h1 h2 = (1, 0)
  | winsOver h2 h1 = (0, 1)
  | otherwise        = (0, 0)

combine a b = (fst a + fst b, snd a + snd b)
zip' (a:as) (b:bs) = (a,b) : zip' as bs

pairScore (h1, h2) = computeScore h1 h2

score :: [Hand] -> [Hand] -> Score
score h1 h2 =
    foldl combine (0, 0) (pairScore (zip h1 h2))


-- theme: adt, list, function
-- goanna results: 7
-- oracle: true
-- intended fix: 1
-- response time: 5.58024001121521
-- mus size: 8
-- ghc loc: 1
-- ghc diagnosis: 1
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 3
-- goanna3: 4
-- goanna4: 5
-- goanna5: 5