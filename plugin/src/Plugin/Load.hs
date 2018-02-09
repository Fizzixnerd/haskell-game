{-# LANGUAGE ScopedTypeVariables #-}

module Plugin.Load where

import Data.Functor
import System.Directory
import GHC
import GHC.Paths
import DynFlags
import Unsafe.Coerce

type ModName = String
type ValName = String

loadPlugin :: FilePath -> ModName -> ValName -> IO a
loadPlugin dir modName value = do
  withCurrentDirectory dir $
    defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $ do
  
    dynFlags <- getSessionDynFlags
    setSessionDynFlags $ dynamicTooMkDynamicDynFlags $ dynFlags { importPaths = [modName] ++ importPaths dynFlags }
    sequence [guessTarget modName Nothing] >>= setTargets
    load LoadAllTargets
    setContext [IIDecl $ simpleImportDecl $ mkModuleName modName]
    fetched <- compileExpr (modName ++ "." ++ value)
    return (unsafeCoerce fetched :: a)
