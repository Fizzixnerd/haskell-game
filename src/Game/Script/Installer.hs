{-# LANGUAGE NoImplicitPrelude #-}

module Game.Script.Installer where

import ClassyPrelude
import Game.Types
import Control.Lens hiding (cons)

-- -- | FIXME: This function barely does anything.  It needs to do a hell
-- -- of a lot more, like register events and so on.  It also needs to
-- -- check for its superscripts
scriptInstall :: Script s -> Game s ()
scriptInstall s = do
  s ^. scriptOnInit
  s ^. scriptOnLoad
  gameStateActiveScripts %= cons s
