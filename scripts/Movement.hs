{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Movement where

import ClassyPrelude
import Game.Types
import qualified Reactive.Banana.Combinators as B
import Control.Lens
import qualified Linear as L
import qualified Graphics.UI.GLFW as G
import GHC.Float (double2Float)


keyToMovement :: G.Key -> Maybe Movement
keyToMovement G.Key'W = Just MoveForward
keyToMovement G.Key'A = Just MoveLeft
keyToMovement G.Key'S = Just MoveBackward
keyToMovement G.Key'D = Just MoveRight
keyToMovement G.Key'LeftShift = Just MoveDown
keyToMovement G.Key'Space = Just MoveUp
keyToMovement _ = Nothing

mouseToMovement :: (G.Window, Double, Double) -> Movement
mouseToMovement (_, x, y) = MoveCameraDir x y

moveCamera :: Float -> Float -> Movement -> Camera -> Camera
moveCamera _ t MoveLeft            = translateCameraRelative (L.V3 (negate t) 0 0)
moveCamera _ t MoveRight           = translateCameraRelative (L.V3 t 0 0)
moveCamera _ t MoveForward         = translateCameraRelative (L.V3 0 0 (negate t))
moveCamera _ t MoveBackward        = translateCameraRelative (L.V3 0 0 t)
moveCamera _ t MoveUp              = translateCameraRelative (L.V3 0 t 0)
moveCamera _ t MoveDown            = translateCameraRelative (L.V3 0 (negate t) 0)
moveCamera o _ (MoveCameraDir x y) = rotateCamera q
  where
    q = mousePosToRot o x y

rotateCamera :: (Float, Float) -> Camera -> Camera
rotateCamera (dhor, dver) cam = cam & cameraOrientation %~ go
  where
    go (hor, ver) = (hor + dhor, max (-pi/2) . min (pi/2) $ ver + dver)

translateCameraRelative :: L.V3 Float -> Camera -> Camera
translateCameraRelative v cam = cam & cameraPosition +~ vrel
  where
    vrel = L.rotate (L.axisAngle (L.V3 0 1 0) (fst . _cameraOrientation $ cam)) v

mousePosToRot :: Float -> Double -> Double -> (Float, Float)
mousePosToRot mouseSpeed x y = (xrot, yrot)
  where
    xrot = mouseSpeed * double2Float (1920/2 - x)
    yrot = mouseSpeed * double2Float (1080/2 - y)

script :: Script
script = defaultScript
  { _scriptOnInit = \gs ->
      let eMovement = B.unionWith const 
                      (B.filterJust (
                          (\(_,k,_,_,_) -> 
                             keyToMovement k) <$> (gs ^. gameStateKeyEvent))) (mouseToMovement <$> (gs ^. gameStateMousePosEvent))
      in
        gs & gameStateEndoRegister %~ registerEndo "movement" 
        ((\m gs_ -> gs_ & gameStateCamera %~ moveCamera (0.005/60) (3/60) m) <$> eMovement)
  }

