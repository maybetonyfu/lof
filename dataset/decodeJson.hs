
module Task where 

data JValue = JString String
  | JNumber Double 
  | JBool Bool
  | JNull 
  | JObject [(String, JValue)]
  | JArray  [JValue]

getString :: JValue -> Maybe String 
getString (JString s) = Just s
getString _           = Nothing

getBool :: JValue -> Maybe Bool 
getBool (JBool b) = Just b 
getBool _         = Nothing 

getNumber :: JValue -> Maybe Double
getNumber (JNumber n) = Just n
getNumber _           = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject js = case js of
   JObject xs -> Just xs
   _          -> Nothing 


getArray :: JValue -> Maybe [JValue]
getArray js = case js of
  JArray xs -> Just xs
  _         -> Nothing 

isNull :: JValue -> Bool 
isNull JNull = True
isNull _     = False 

renderJValue :: JValue -> String
renderJValue (JString s)   = show s
renderJValue (JNumber n)   = show n
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue JNull         = "null"

renderJValue (JObject o) = "{" ++ renderPairs o ++ "}"
renderJValue (JArray a) = "[" ++ renderPairs a ++ "]"

renderPair :: (String, JValue) -> String
renderPair (k,v)   = show k ++ ": " ++ renderJValue v

renderPairs [] = ""
renderPairs [p] = renderPair p
renderPairs (p:ps) = renderPair p ++ "," ++ renderPairs ps


renderArrayValues [] = ""
renderArrayValues [v] = renderJValue v
renderArrayValues (v:vs) = renderJValue v ++ "," ++ renderArrayValues vs

-- theme: adt
-- goanna results: 15
-- oracle: true
-- intended fix: 1