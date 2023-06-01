standardTrans z =
  case z of
    "shorttitle" -> ["short"]
    "sorttitle" -> ["sorted"]
    "indextitle" -> ["index"]
    "indexsorttitle" -> ["index", "sorted"]
    _ -> z
-- bookTrans :: String -> [String]
bookTrans z =
  case z of
    "title" -> ["booktitle"]
    "subtitle" -> ["booksubtitle"]
    "titleaddon" -> ["booktitleaddon"]
    "shorttitle" -> []
    "sorttitle" -> []
    "indextitle" -> []
    "indexsorttitle" -> []
    _ -> [z]
transformKey x y "author"
  | elem x ["mvbook", "book"] =
    ["bookauthor", "author"]
transformKey _ _ x = [x]


-- theme: list
-- goanna results: 4
-- oracle: true
-- intended fix: 1
-- response time: 4.179111957550049
-- mus size: 9
-- ghc loc: 1
-- ghc diagnosis: 1
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 2
-- goanna3: 7
-- goanna4: 11
-- goanna5: 11
