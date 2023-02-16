module Test where

data Options = Options
  { optionsVerbose :: !Bool
  }


data AST =  AST {
  file :: String,
  moduleName :: String,
  ast :: Module (Scoped SrcSpanInfo)
} deriving (Show, Generic)

data Output = 
  ParseError [(String, SrcLoc)] | ParseOK [AST] deriving (Show, Generic)
