module Util where

import Control.Wire
import Game.Types
import Game.Events

reloadPlugins :: GameWire s a ()
reloadPlugins = rldPlgns <<< keyP
  where rldPlgns = mkGen_ $ const $ Right <$>
                   (return ())
