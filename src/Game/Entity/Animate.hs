module Game.Entity.Animate where

import Game.Types
import Control.Lens
import Game.Entity.Gfx
import Game.Entity.Sfx
import Game.Entity.Lfx

animateEntity :: Entity s -> Game s (Entity s)
animateEntity e = do
  cam <- use gameStateCamera
  e' <- scriptEntity e
  playEntity e'
  drawEntity cam e'
  return e'

