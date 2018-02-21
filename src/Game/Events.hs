{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Events where

import           Control.Arrow
import           Control.Wire
import           FRP.Netwire
import qualified FRP.Netwire.Input as N
import           ClassyPrelude
import           Control.Lens
import           Game.Graphics.Rendering
import           Game.Types
import           Game.Script.Loader
import           Game.Script.Installer
import           Graphics.Binding
import           Game.Entity.Player
import           Game.World.Physics
import qualified Linear                       as L
import           GHC.Float (double2Float)

mousePosToRot :: Float -> MousePos -> MousePos -> Double -> (Float, Float)
mousePosToRot mouseSpeed (MousePos (L.V2 newx newy)) (MousePos (L.V2 oldx oldy)) dt =
  (xrot, yrot)
  where
    xrot = mouseSpeed * double2Float (dt * (oldx - newx))
    yrot = mouseSpeed * double2Float (dt * (oldy - newy))

rotateCamera :: (Float, Float) -> Camera -> Camera
rotateCamera (dhor, dver) cam = cam & cameraOrientation %~ go
  where
    go (hor, ver) = (hor + dhor, max (-pi/2) . min (pi/2) $ ver + dver)

camera :: GameWire s a ()
camera = (arr (const ()) >>> N.cursorMode N.CursorMode'Disabled)
         --> (rotCam <<< N.mouseDelta)
  where
    rotCam = mkGen_ $ \(x, y) -> Right <$> (gameStateCamera %= rotateCamera (-x, -y))

moveForward :: GameWire s a ()
moveForward = mvFwd <<< key'w
  where
    mvFwd = mkGen_ $ const $ Right <$>
            (gameStateCamera . cameraPosition += L.V3 0 0 (-0.1))

moveBackward :: GameWire s a ()
moveBackward = mvBwd <<< key's
  where
    mvBwd = mkGen_ $ const $ Right <$>
            (gameStateCamera . cameraPosition += L.V3 0 0 0.1)

moveLeft :: GameWire s a ()
moveLeft = mvLft <<< key'a
  where
    mvLft = mkGen_ $ const $ Right <$>
            (gameStateCamera . cameraPosition += L.V3 (-0.1) 0 0)

moveRight :: GameWire s a ()
moveRight = mvRgt <<< key'd
  where
    mvRgt = mkGen_ $ const $ Right <$>
            (gameStateCamera . cameraPosition += L.V3 0.1 0 0)

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
