{-# LANGUAGE ScopedTypeVariables #-}

module Plugin.Load where

import Data.Functor
import System.Directory
import GHC
import GHC.Paths
import DynFlags
import Unsafe.Coerce
import Plugin.Types

loadPlugin :: Plugin -> IO a
loadPlugin (Plugin dir modName value) = do
  withCurrentDirectory dir $
    defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $ withCleanupSession $ do

    dynFlags <- getSessionDynFlags
    setSessionDynFlags $ dynamicTooMkDynamicDynFlags $ dynFlags 
      { importPaths = [modName] ++ importPaths dynFlags
      , hscTarget = HscAsm
      , ghcLink = LinkBinary
      , ghcMode = CompManager
      }
    sequence [guessTarget modName Nothing] >>= setTargets
    load LoadAllTargets
    setContext [IIDecl $ simpleImportDecl $ mkModuleName modName]
    fetched <- compileExpr (modName ++ "." ++ value)
    return (unsafeCoerce fetched :: a)
