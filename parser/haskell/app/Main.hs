{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import RIO
import Types
import Process hiding (main)
import RIO.Process
import System.Environment
import qualified RIO.Map as M

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "argument needed: basedir "
    (x:_) -> do
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
                  prgPath = x,
                  prgFileList = initFileList,
                  prgAST = initAST,
                  prgResult = initResult,
                  prgNames = initNames,
                  prgScopedAST = initScopedAST
                }
        runRIO program plan