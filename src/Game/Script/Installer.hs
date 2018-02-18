{-# LANGUAGE NoImplicitPrelude #-}

module Game.Script.Installer where

import ClassyPrelude
import Game.Types
import Control.Lens hiding (cons)

-- -- | FIXME: This function barely does anything.  It needs to do a hell
-- -- of a lot more, like register events and so on.  It also needs to
-- -- check for its superscripts
-- scriptInstall :: Script -> GameState -> Game GameState
-- scriptInstall s gs = do
--   gs'  <- s ^. scriptOnInit $ gs
--   gs'' <- s ^. scriptOnLoad $ gs'
--   let newEndoRegister = foldr (\(eventName, endoName, endo) endoR ->
--                                  registerEndoByName
--                                 eventName
--                                 endoName
--                                 endo
--                                 (gs ^. gameStateEventRegister)
--                                 endoR)
--                         (gs'' ^. gameStateEndoRegister)
--                         (s ^. scriptOnEvent)
--   return $ gs'' & gameStateActiveScripts %~ cons s
--                 & gameStateEndoRegister .~ newEndoRegister
