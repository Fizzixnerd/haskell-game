{-# LANGUAGE ScopedTypeVariables #-}

module Game.Script.Loader where

import Game.Types
import qualified Plugin.Load as PL
import Control.Lens

scriptLoad :: ScriptName -> IO Script
scriptLoad sn = do
  (script :: Script) <- PL.loadPlugin (sn^.scriptNamePath) (sn^.scriptNameMainModule) "script"
  return $ script & scriptName .~ sn

