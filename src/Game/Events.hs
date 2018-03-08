{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Events where

import           Control.Arrow
import           Control.Wire
import qualified FRP.Netwire.Input as N
import           Foreign.C.Types
import           ClassyPrelude
import           Control.Lens
import           Game.Types
import           Game.Entity.Player
import           Game.Entity.Camera
import qualified Physics.Bullet as P
import           Graphics.Binding
import qualified Linear as L
import qualified Sound.OpenAL as AL
import           Game.Wires

zoomCamera :: GameEffectWire s
zoomCamera = effectWire $ do
  cam <- use gameStateCamera
  disp <- getCameraDisplacementFromTarget cam
  let dist = L.norm disp
      rs   = (cam ^. cameraPreferredDistance) - dist
  t <- liftIO $ P.coAllocateWorldTransform =<< cam ^. cameraController . to P.getGhostObject
  (x, y, z) <- liftIO $ P.getOrigin t
  liftIO $ P.del t
  AL.listenerPosition AL.$= AL.Vertex3 x y z
  setCameraRadialSpeed cam rs
  cameraLookAtTarget cam

turnPlayer :: GameEffectWire s
turnPlayer = effectWire $ join $
  setPlayerOrientation <$> use gameStatePlayer <*> (use gameStateCamera >>= getCameraOrientation)

rotateCamera :: MonadIO m => (Float, Float) -> Camera s -> m ()
rotateCamera (dhor, dver) cam = do
  costheta <- getCameraInclinationCos cam
  setCameraPolarSpeed cam (clampy costheta)
  setCameraAzimuthalSpeed cam (CFloat dhor)
  where
    clampy theta
      | theta > 0.9    = CFloat $ max 0 dver
      | theta < (-0.9) = CFloat $ min 0 dver
      | otherwise      = CFloat dver

camera :: GameEffectWire s
camera = N.cursorMode N.CursorMode'Reset --> camWire
  where
    rotCam = mkGen_ $ \(x, y) -> Right <$> do
      cam <- use gameStateCamera
      rotateCamera (-x * 10, -y * 10)  cam
    camWire = passWire $ rotCam <<< N.mouseMickies

movePlayer :: GameWire s (L.V3 CFloat) ()
movePlayer = mkGen_ $ \dir -> Right <$> do
  p <- use gameStatePlayer
  setPlayerLinearVelocity p dir

playerHorizontalMovement :: GameWire s a (L.V3 CFloat)
playerHorizontalMovement = (\v -> 0.1 * recip (L.norm v) L.*^ v) <$> solderWire (+) zwire xwire
  where
    fwd = mkConstM $ join $ uses gameStateCamera getCameraForward
    lft = mkConstM $ join $ uses gameStateCamera getCameraLeft
    xwire = xorWire (keyA >>> lft) (keyD >>> (negate <$> lft))
    zwire = xorWire (keyW >>> fwd) (keyS >>> (negate <$> fwd))

close :: GameEffectWire s
close = cls <<< keyEsc
  where
    cls = effectWire $ gameStateShouldClose .= True

jump :: GameEffectWire s
jump = jmp <<< keySpace
  where
    jmp = effectWire $ do
      p <- use $ gameStatePlayer . playerController
      liftIO $ P.jump p

basicEventStream :: (Fractional a, HasTime t s) => GameWire s a (Event a)
basicEventStream = periodic 4 . timeF

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

keyP :: GameWire s a a
keyP = N.keyPressed Key'P

keyEsc :: GameWire s a a
keyEsc = N.keyPressed Key'Escape

keySpace :: GameWire s a a
keySpace = N.keyPressed Key'Space
