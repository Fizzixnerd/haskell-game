{-# LANGUAGE ScopedTypeVariables #-}

module Game.Script.Loader where

import Game.Types
import qualified Plugin.Types as PL
import qualified Plugin.Load as PL
import Control.Lens

scriptLoad :: ScriptName -> IO (Script s)
scriptLoad sn = do
  (script :: (Script s)) <- PL.loadPlugin $ PL.Plugin (sn^.scriptNamePath) (sn^.scriptNameMainModule) "script"
  return $ script & scriptName .~ sn

