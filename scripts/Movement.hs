{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}

module Movement where

import           ClassyPrelude
import           Control.Arrow
import           Control.Wire 
import           Control.Lens
import           Game.Entity.Player
import           Game.Events
import           Game.Types
import qualified Linear as L

moveForward :: GameWire s a ()
moveForward = mvFwd <<< key'w
  where
    mvFwd = mkGen_ $ const $ Right <$>
            (do
                p <- use gameStatePlayer
                setPlayerLinearVelocity p $ L.V3 0 0 0.1)

moveBackward :: GameWire s a ()
moveBackward = mvBwd <<< key's
  where
    mvBwd = mkGen_ $ const $ Right <$>
            (do
                p <- use gameStatePlayer
                setPlayerLinearVelocity p $ L.V3 0 0 (-0.1))
