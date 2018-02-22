{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Events where

import           Control.Arrow
import           Control.Wire
import qualified FRP.Netwire.Input as N
import           ClassyPrelude
import           Control.Lens
import           Game.Types
import           Game.Entity.Player
import           Game.Entity.Camera
import qualified Physics.Bullet as P
import           Graphics.Binding
import qualified Linear                       as L
import           GHC.Float (double2Float)

mousePosToRot :: Float -> MousePos -> MousePos -> Double -> (Float, Float)
mousePosToRot mouseSpeed (MousePos (L.V2 newx newy)) (MousePos (L.V2 oldx oldy)) dt =
  (xrot, yrot)
  where
    xrot = mouseSpeed * double2Float (dt * (oldx - newx))
    yrot = mouseSpeed * double2Float (dt * (oldy - newy))

zoomCamera :: GameWire s a ()
zoomCamera = mkGen_ $ const $ Right <$> 
             (do
                 c <- use gameStateCamera
                 d <- getCameraDisplacementFromTarget c
                 rhat <- getCameraRHat c
                 let cv = - d + (rhat L.^* (c ^. cameraPreferredDistance))
                 setCameraLinearVelocity cv c)

-- rotateCamera :: (Float, Float) -> Camera -> Camera
-- rotateCamera (dhor, dver) cam = cam & cameraOrientation %~ go
--   where
--     go (hor, ver) = (hor + dhor, max (-pi/2) . min (pi/2) $ ver + dver)

-- camera :: GameWire s a ()
-- camera = (arr (const ()) >>> N.cursorMode N.CursorMode'Disabled)
--          --> (rotCam <<< N.mouseDelta)
--   where
--     rotCam = mkGen_ $ \(x, y) -> Right <$> (gameStateCamera %= rotateCamera (-x, -y))

-- moveForward :: GameWire s a ()
-- moveForward = mvFwd <<< keyW
--   where
--     mvFwd = mkGen_ $ const $ Right <$>
--             (do
--                 p <- use gameStatePlayer
--                 setPlayerLinearVelocity p $ L.V3 0 0 0.1)

-- moveBackward :: GameWire s a ()
-- moveBackward = mvBwd <<< keyS
--   where
--     mvBwd = mkGen_ $ const $ Right <$>
--             (do
--                 p <- use gameStatePlayer
--                 setPlayerLinearVelocity p $ L.V3 0 0 (-0.1))

moveLeft :: GameWire s a ()
moveLeft = mvLft <<< keyA
  where
    mvLft = mkGen_ $ const $ Right <$>
            (do
                p <- use gameStatePlayer
                setPlayerLinearVelocity p $ L.V3 0.1 0 0)

moveRight :: GameWire s a ()
moveRight = mvRgt <<< keyD
  where
    mvRgt = mkGen_ $ const $ Right <$>
            (do
                p <- use gameStatePlayer
                setPlayerLinearVelocity p $ L.V3 (-0.1) 0 0)

close :: GameWire s a ()
close = cls <<< keyEsc
  where
    cls = mkGen_ $ const $ Right <$>
          (gameStateShouldClose .= True)

jump :: GameWire s a ()
jump = jmp <<< keySpace
  where
    jmp = mkGen_ $ const $ Right <$>
          (do
              p <- use $ gameStatePlayer . playerController
              liftIO $ P.jump p)

mouseL :: GameWire s a a
mouseL = N.mousePressed MouseButton'1

mouseR :: GameWire s a a
mouseR = N.mousePressed MouseButton'2

keyW :: GameWire s a a
keyW = N.keyPressed Key'W

keyA :: GameWire s a a
keyA = N.keyPressed Key'A

keyS :: GameWire s a a
keyS = N.keyPressed Key'S

keyD :: GameWire s a a
keyD = N.keyPressed Key'D

keyEsc :: GameWire s a a
keyEsc = N.keyPressed Key'Escape

keySpace :: GameWire s a a
keySpace = N.keyPressed Key'Space
