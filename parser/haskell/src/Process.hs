{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Process where

import Dir
import Language.Haskell.Exts
import Language.Haskell.Names
import RIO
import RIO.FilePath
import qualified RIO.Map as M
import RIO.Process
import qualified RIO.Text as T
import Types
import Util
import Data.List (zipWith3)
import qualified RIO.ByteString.Lazy as BS
import           Data.Aeson

makeFileList :: RIO Program ()
makeFileList = do
  rootDir <- view pathL
  files <- getFilesRecursive rootDir
  let normFiles = fmap normalise files
  let hsFiles = filter ((== ".hs") . takeExtensions) normFiles
  let relFiles = map (makeRelative rootDir) hsFiles
  fileListRef <- view fileListL
  writeIORef fileListRef relFiles

parsePrograms :: RIO Program ()
parsePrograms = do
  logDebug "Stage: Parsing program"
  fileList <- readIORefFromLens fileListL
  results <- forM fileList parseProgram
  if null (lefts results)
    then do
      astRef <- view astL
      writeIORef astRef (rights results)
    else do
      resultRef <- view resultL
      writeIORef resultRef (Just (lefts results))

parseProgram :: FilePath -> RIO Program (Either (String, SrcLoc) (Module SrcSpanInfo))
parseProgram file = do
  rootDir <- view pathL
  fileContent <- readFileUtf8 (normalise rootDir </> normalise file)
  logDebug (display fileContent)
  let pResult = parseModuleWithMode parseMode (T.unpack fileContent)
      parseMode = defaultParseMode {parseFilename = file}
  case pResult of
    ParseOk hModule -> do
      return (Right hModule)
    ParseFailed srcLoc message -> do
      return $ Left (message, srcLoc)

ensureModuleNames :: RIO Program ()
ensureModuleNames = do
  fileNames <- readIORefFromLens fileListL
  modules <- readIORefFromLens astL
  let altNames = map guessModuleName fileNames
  let withProperNames = zipWith fixModuleName altNames modules
  astRef <- view astL
  writeIORef astRef withProperNames

annotateScopes :: RIO Program ()
annotateScopes = do
  baseEnv <- liftIO loadBase
  modules <- readIORefFromLens astL
  let names = resolve modules baseEnv
  asts <- readIORefFromLens astL
  let newASTs = map (annotate names) asts
  scopedAstRef <- view scopedAstL
  writeIORef scopedAstRef newASTs

report :: RIO Program ()
report = do 
  result <- readIORefFromLens resultL
  fileNames <- readIORefFromLens fileListL
  scopedAst <- readIORefFromLens scopedAstL
  let maybeMdNames = map pickModuleName scopedAst
  let mdNames = map (fromMaybe (error "Should not happen")) maybeMdNames
  let output = 
          case result of
            Just errs -> ParseError errs
            Nothing -> ParseOK $ zipWith3 AST fileNames mdNames scopedAst
  let reportJson = encode output
  BS.putStr reportJson 


plan :: RIO Program ()
plan = do
  makeFileList
  parsePrograms
  ensureModuleNames
  annotateScopes
  report

main :: IO ()
main = do
  lo <- logOptionsHandle stderr False
  withLogFunc lo $ \lf -> do
    processContext <- mkDefaultProcessContext
    initAST <- newIORef []
    initScopedAST <- newIORef []
    initFileList <- newIORef []
    initResult <- newIORef Nothing

    initNames <- newIORef M.empty
    let program =
          Program
            { prgLogFunc = lf,
              prgProcessContext = processContext,
              prgOptions = Options {optionsVerbose = False},
              prgPath = "c:/Users/sfuu0016/Projects/haskell-tool/test",
              prgFileList = initFileList,
              prgAST = initAST,
              prgResult = initResult,
              prgNames = initNames,
              prgScopedAST = initScopedAST
            }
    runRIO program plan