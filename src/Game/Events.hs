{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
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
import qualified Linear                       as L


-- Is the identity if only one wire is producing. If both are, it merges the results with merge.
-- This steps both wires.
solderWire :: (Monoid e, Monad m) => (b -> b -> b) -> Wire s e m a b -> Wire s e m a b -> Wire s e m a b
solderWire merge' w1 w2 = WGen $ \s eea -> do
  (eeb1, _) <- stepWire w1 s eea
  (eeb2, _) <- stepWire w2 s eea
  return (merger eeb1 eeb2, solderWire merge' w1 w2)
  where
    merger = \case
      Left err -> left (mappend err)
      Right x  -> const (Right x) ||| (Right . merge' x)

-- If both are producing or both are inhibited, then it inhibits.
-- Otherwise it acts like the producing one.
-- This thing has to step both wires.
xorWire :: (Monoid e, Monad m) => Wire s e m a b -> Wire s e m a b -> Wire s e m a b
xorWire w1 w2 = WGen $ \s eea -> do
  (eeb1, _) <- stepWire w1 s eea
  (eeb2, _) <- stepWire w2 s eea
  return (inverter eeb1 eeb2, xorWire w1 w2)
  where
    inverter = \case
      Left err -> left (mappend err)
      Right x  -> const (Right x) ||| const (Left mempty)

zoomCamera :: GameWire s a ()
zoomCamera = mkGen_ $ const $ Right <$> do
  cam <- use gameStateCamera
  disp <- getCameraDisplacementFromTarget cam
  let dist = L.norm disp
      rs   = (cam ^. cameraPreferredDistance) - dist
  setCameraRadialSpeed cam rs
  cameraLookAtTarget cam

turnPlayer :: GameWire s a ()
turnPlayer = mkGen_ $ const $ Right <$> do
  cam <- use gameStateCamera
  target <- use $ gameStatePlayer
  orientation <- getCameraOrientation cam
  setPlayerOrientation target orientation

rotateCamera :: MonadIO m => (Float, Float) -> Camera -> m ()
rotateCamera (dhor, dver) cam = do
  setCameraPolarSpeed cam (CFloat dver)
  setCameraAzimuthalSpeed cam (CFloat dhor)

camera :: GameWire s a ()
camera = (arr (const ()) >>> N.cursorMode N.CursorMode'Reset)
         --> (rotCam <<< N.mouseMickies)
  where
    rotCam = mkGen_ $ \(x, y) -> Right <$> do
      cam <- use gameStateCamera
      rotateCamera (-x * 10, -y * 10)  cam

movePlayer :: GameWire s (L.V3 CFloat) ()
movePlayer = mkGen_ $ \dir -> Right <$> do
  p <- use gameStatePlayer
  setPlayerLinearVelocity p dir

playerHorizontalMovement :: GameWire s a (L.V3 CFloat)
playerHorizontalMovement = (\v -> 0.1 * recip (L.norm v) L.*^ v) <$> solderWire (+) zwire xwire
  where
    fwd = pure $ L.V3 0 0 1
    bwd = pure $ L.V3 0 0 (-1)
    lft = pure $ L.V3 1 0 0
    rgt = pure $ L.V3 (-1) 0 0
    zwire = xorWire (keyW >>> fwd) (keyS >>> bwd)
    xwire = xorWire (keyA >>> lft) (keyD >>> rgt)

close :: GameWire s a ()
close = cls <<< keyEsc
  where
    cls = mkGen_ $ const $ Right <$> (gameStateShouldClose .= True)

jump :: GameWire s a ()
jump = jmp <<< keySpace
  where
    jmp = mkGen_ $ const $ Right <$> do
      p <- use $ gameStatePlayer . playerController
      liftIO $ P.jump p

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
