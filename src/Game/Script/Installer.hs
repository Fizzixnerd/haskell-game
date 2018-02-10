{-# LANGUAGE NoImplicitPrelude #-}

module Game.Script.Installer where

import ClassyPrelude
import Game.Types
import Control.Lens hiding (cons)

-- | FIXME: This function barely does anything.  It needs to do a hell
-- of a lot more, like register events and so on.
scriptInstall :: Script -> GameState -> Game GameState
scriptInstall s gs = do
  gs' <- s ^. scriptOnInit $ gs
  gs'' <- s ^. scriptOnLoad $ gs'
  let gs''' = gs'' & gameStateActiveScripts %~ cons s
  return gs'''
