
data XML = XML Position Part
data Position = Top | Bottom | Left | Right

type Name = String

data Part =
     Element Name [Attribute] [XML]
   | Comment String
   | Text String

getPart :: XML -> Part
getPart (XML pos part) = part


printXML (Element name [attributs] xmls) =
  "<" ++ name ++ ">"
  ++ mconcat (map printXML xmls)
  ++ "</" ++ name ++ ">"
printXML (Text text) = text

-- theme: adt
-- goanna results: 4
-- oracle: true
-- intended fix: 4
-- response time: 0.49406981468200684
-- mus size: 5
-- ghc loc: 0
-- ghc diagnosis: 0
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 2
-- goanna3: 3
-- goanna4: 4
-- goanna5: 4

