
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


