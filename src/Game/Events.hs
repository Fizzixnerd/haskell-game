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
      rs   = (\(CFloat x) -> x) (cam ^. cameraPreferredDistance) - dist
  t <- liftIO $ P.coAllocateWorldTransform $ cam ^. cameraController
  (x, y, z) <- liftIO $ P.getOrigin t
  liftIO $ P.del t
  AL.listenerPosition AL.$= AL.Vertex3 x y z
  srad <- getCameraRadialSpeed cam
  setCameraRadialForce cam (10 * rs - srad)
  sazi <- getCameraAzimuthalSpeed cam
  setCameraAzimuthalForce cam  (- sazi)
  spol <- getCameraPolarSpeed cam
  setCameraPolarForce cam (- spol)

-- turnPlayer :: GameEffectWire s
-- turnPlayer = effectWire $ do
--   p <- use gameStatePlayer
--   c <- use gameStateCamera
--   o <- getCameraOrientation c
--   setPlayerOrientation p o

rotateCamera :: MonadIO m => (Float, Float) -> Camera s -> m ()
rotateCamera (dhor, dver) cam = do
  costheta <- getCameraInclinationCos cam
  setCameraPolarForce cam (clampy costheta)
  setCameraAzimuthalForce cam dhor
  where
    clampy theta
      | theta > 0.9    = max 0 dver
      | theta < (-0.9) = min 0 dver
      | otherwise      = dver

camera :: GameEffectWire s
camera = camWire
  where
    rotCam = mkGen_ $ \(x, y) -> Right <$> do
      cam <- use gameStateCamera
      rotateCamera (-x * 100, -y * 100) cam
    camWire = passWire $ rotCam <<< N.mouseMickies

movePlayer :: GameWire s (L.V3 Float) ()
movePlayer = mkGen_ $ \dir -> Right <$> do
  p <- use gameStatePlayer
  let force_ = 100 * dir
  playerApplyForce p force_

playerHorizontalMovement :: GameWire s a (L.V3 Float)
playerHorizontalMovement = (\v -> 0.1 * recip (L.norm v) L.*^ v) <$> solderWire (+) zwire xwire
  where
    fwd = mkConstM $ join $ uses gameStateCamera getCameraForward
    rgt = mkConstM $ join $ uses gameStateCamera getCameraLeft
    xwire = xorWire (keyA >>> negate <$> rgt) (keyD >>> rgt)
    zwire = xorWire (keyW >>> fwd) (keyS >>> (negate <$> fwd))

close :: GameEffectWire s
close = cls <<< keyEsc
  where
    cls = effectWire $ gameStateShouldClose .= True

jump :: GameEffectWire s
jump = jmp <<< keySpace
  where
    jmp = effectWire $ do
      p <- use gameStatePlayer
      playerJump p

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
