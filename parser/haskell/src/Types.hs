{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import Data.Aeson hiding (Options)
import JSON()
import Language.Haskell.Exts
import Language.Haskell.Names
import RIO
import RIO.Process

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data AST = AST
  { file :: String,
    moduleName :: String,
    ast :: Module (Scoped SrcSpanInfo)
  }
  deriving (Show, Generic)

data Output
  = ParseError [(String, SrcLoc)]
  | ParseOK [AST]
  deriving (Show, Generic)

instance ToJSON AST

instance ToJSON Output

data Program = Program
  { prgLogFunc :: !LogFunc,
    prgProcessContext :: !ProcessContext,
    prgPath :: !String,
    prgOptions :: !Options,
    prgResult :: IORef (Maybe [(String, SrcLoc)]),
    prgFileList :: IORef [String],
    prgAST :: IORef [Module SrcSpanInfo],
    prgScopedAST :: IORef [Module (Scoped SrcSpanInfo)],
    prgNames :: IORef Environment
  }

instance HasLogFunc Program where
  logFuncL = lens prgLogFunc (\x y -> x {prgLogFunc = y})

instance HasProcessContext Program where
  processContextL = lens prgProcessContext (\x y -> x {prgProcessContext = y})

class HasPath a where
  pathL :: Lens' a String

instance HasPath Program where
  pathL = lens prgPath (\x y -> x {prgPath = y})

class HasFileList a where
  fileListL :: Lens' a (IORef [String])

instance HasFileList Program where
  fileListL = lens prgFileList (\x y -> x {prgFileList = y})

class HasAST a where
  astL :: Lens' a (IORef [Module SrcSpanInfo])

instance HasAST Program where
  astL = lens prgAST (\x y -> x {prgAST = y})

class HasScopedAST a where
  scopedAstL :: Lens' a (IORef [Module (Scoped SrcSpanInfo)])

instance HasScopedAST Program where
  scopedAstL = lens prgScopedAST (\x y -> x {prgScopedAST = y})

class HasResult a where
  resultL :: Lens' a (IORef (Maybe [(String, SrcLoc)]))

instance HasResult Program where
  resultL = lens prgResult (\x y -> x {prgResult = y})

class HasNames a where
  namesL :: Lens' a (IORef Environment)

instance HasNames Program where
  namesL = lens prgNames (\x y -> x {prgNames = y})

readIORefFromLens :: Lens' env (IORef b) -> RIO env b
readIORefFromLens l = do
  h <- view l
  readIORef h
