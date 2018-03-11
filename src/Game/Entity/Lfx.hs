{-# LANGUAGE NoImplicitPrelude #-}

module Game.Entity.Lfx where

import Game.Types
import Control.Lens
import ClassyPrelude

scriptEntity :: Entity s -> Game s (Entity s)
scriptEntity e = case e ^. entityLogic of
  Nothing -> return e
  Just lfx -> foldlM (\ent scr -> scr ent) e (lfx ^. lfxScripts)

