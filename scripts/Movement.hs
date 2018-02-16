{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}

module Movement where

import ClassyPrelude
import Game.Types
import Game.Events
import Game.Entity.Player
import Control.Lens
import Foreign.C.Types
import qualified Linear as L
import           GHC.Float (double2Float)
import qualified Graphics.UI.GLFW as G

relativeDir :: Camera -> L.V3 Float -> L.V3 Float
relativeDir cam = L.rotate (L.axisAngle (L.V3 0 1 0) (fst . _cameraOrientation $ cam))

changeMovement :: MonadIO m => G.Key -> G.KeyState -> Camera -> Player -> m ()
changeMovement k ks cam p@Player {..} = do
  let op_ = case ks of
              G.KeyState'Pressed -> (+)
              G.KeyState'Released -> (-)
              G.KeyState'Repeating -> const
      dir :: L.V3 CFloat
      dir = case k of
              G.Key'W -> L.V3 0 0 (negate 1)
              G.Key'A -> L.V3 (negate 1) 0 0
              G.Key'S -> L.V3 0 0 1
              G.Key'D -> L.V3 1 0 0
              G.Key'LeftShift -> L.V3 0 (negate 1) 0
              G.Key'Space     -> L.V3 0 1 0
              _       -> L.V3 0 0 0
  lv <- getPlayerLinearVelocity p
  setPlayerLinearVelocity p (L.normalize $ lv `op_` (fmap CFloat . relativeDir cam . fmap (\(CFloat x) -> x) $ dir))

script :: Script
script = defaultScript
  { _scriptOnInit = installKeyEventListener keyhandle "movement"
                     }
  where
    keyhandle (_, k, _, ks, _) gs = do
      changeMovement k ks (gs ^. gameStateCamera) (gs ^. gameStatePlayer)
      return gs
