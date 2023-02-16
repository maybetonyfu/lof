{-# LANGUAGE NoImplicitPrelude #-}
module Util where

import RIO
import RIO.FilePath
import Language.Haskell.Exts
import Data.Char (toUpper, isAlphaNum)
import Data.List (intercalate)

plus2 :: Int -> Int
plus2 = (+ 2)

pickModuleName :: Module a -> Maybe String
pickModuleName (Module _ (Just (ModuleHead _ (ModuleName _ n) _ _)) _ _ _) = Just n
pickModuleName (Module _ Nothing _ _ _) = Nothing
pickModuleName _ = error "Not a module"

fixModuleName :: String -> Module SrcSpanInfo -> Module SrcSpanInfo
fixModuleName _ m@(Module _ (Just (ModuleHead _ (ModuleName _ _) _ _)) _ _ _) = m
fixModuleName altName (Module l Nothing pragmas imports decls) = 
    let mh = Just (ModuleHead noSrcSpan (ModuleName noSrcSpan altName) Nothing Nothing)
    in Module l mh pragmas imports decls
fixModuleName _ _ = error "Not a module"

capitalize :: String -> String 
capitalize (x:xs) = toUpper x : xs
capitalize [] = []

clean :: String -> String 
clean  = filter isAlphaNum

guessModuleName :: FilePath -> String
guessModuleName path =
    let fragments =  filter (not . null) . map (clean . capitalize) . splitPath . dropExtensions $ path
    in intercalate "." fragments
