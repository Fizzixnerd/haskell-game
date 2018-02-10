{-# LANGUAGE ScopedTypeVariables #-}

module Game.Script.Loader where

import Game.Types
import qualified Plugin.Load as PL
import Control.Lens

loadForeignScript :: ScriptName -> IO Script
loadForeignScript sn = do
  (script :: Script) <- PL.loadPlugin (sn^.scriptNamePath) (sn^.scriptNameMainModule) "script"
  return $ script & scriptName .~ sn

