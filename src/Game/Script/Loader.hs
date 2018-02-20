{-# LANGUAGE ScopedTypeVariables #-}

module Game.Script.Loader where

import Game.Types
import qualified Plugin.Load as PL
import Control.Lens

scriptLoad :: ScriptName -> IO (Script s)
scriptLoad sn = do
  (script :: (Script s)) <- PL.loadPlugin (sn^.scriptNamePath) (sn^.scriptNameMainModule) "script"
  return $ script & scriptName .~ sn

