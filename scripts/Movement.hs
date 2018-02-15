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
import Text.Printf

mousePosToRot :: Float -> Double -> Double -> (Float, Float)
mousePosToRot mouseSpeed x y = (xrot, yrot)
  where
    clampy a b = max a . min b
    xrot = mouseSpeed * clampy (-20) 20 (double2Float (1920/2 - x))
    yrot = mouseSpeed * clampy (-20) 20 (double2Float (1080/2 - y))

rotateCamera :: (Float, Float) -> Camera -> Camera
rotateCamera (dhor, dver) cam = cam & cameraOrientation %~ go
  where
    go (hor, ver) = (hor + dhor, max (-pi/2) . min (pi/2) $ ver + dver)

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
  { _scriptOnInit = installKeyEventListener keyhandle "movement" >=>
                    installMouseEventListener mousehandle "camOrientation" }
  where
    keyhandle (_, k, _, ks, _) gs = do
      changeMovement k ks (gs ^. gameStateCamera) (gs ^. gameStatePlayer)
      return gs
    mousehandle (x, y) gs = liftIO $ do
      printf "Harro senpai:%s\n" (show (x,y))
      return (gs & gameStateCamera %~ camMove)
      where
        camMove = rotateCamera $ mousePosToRot (0.005/60) x y
