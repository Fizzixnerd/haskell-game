{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Game.Events where

import           Control.Arrow
import           Control.Wire
import qualified FRP.Netwire.Input as N
import           Foreign.C.Types
import           ClassyPrelude
import           Control.Lens      as Lens
import           Game.Types
import           Game.Entity.Player
import           Game.Entity.Camera
import qualified Physics.Bullet as P
import           Graphics.Binding
import qualified Linear                       as L
import qualified Sound.OpenAL as AL


-- | Is the identity if only one wire is producing. If both are, it
-- merges the results with merge.  This steps both wires.
solderWire :: (Monoid e, Monad m) => (b -> b -> b) -> Wire s e m a b -> Wire s e m a b -> Wire s e m a b
solderWire merge' w1 w2 = WGen $ \s eea -> do
  (eeb1, _) <- stepWire w1 s eea
  (eeb2, _) <- stepWire w2 s eea
  return (merger eeb1 eeb2, solderWire merge' w1 w2)
  where
    merger = \case
      Left err -> left (mappend err)
      Right x  -> const (Right x) ||| (Right . merge' x)

solderWireM :: (Monoid e, Monad m) => (b -> b -> m b) -> Wire s e m a b -> Wire s e m a b -> Wire s e m a b
solderWireM merge' w1 w2 = WGen $ \s eea -> do
  (eeb1, _) <- stepWire w1 s eea
  (eeb2, _) <- stepWire w2 s eea
  res <- merger eeb1 eeb2
  return (res, solderWireM merge' w1 w2)
  where
    merger = \case
      Left err -> pure . left (mappend err)
      Right x  -> const (pure . Right $ x) ||| (fmap Right . merge' x)

effectiveWire :: Monad m => m c -> Wire s e m a a
effectiveWire act = mkGen_ $ \a -> void act >> return (Right a)

mkMConst :: Monad m => m b -> Wire s e m a b
mkMConst act = mkGen_ $ const $ Right <$> act

-- Example usage for caching:
-- make a thingUpdateWire that when pulsed will return thing.
-- then feed that into steppingWire
-- Also, to future Matt: past Christian sends his regards.

steppingWire :: Wire s e m a a
steppingWire = mkPureN $ \a -> (Right a, mkConst (Right a))

-- | If both are producing or both are inhibited, then it inhibits.
-- Otherwise it acts like the producing one.  This thing has to step
-- both wires.
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
zoomCamera = mkMConst $ do
  cam <- use gameStateCamera
  disp <- getCameraDisplacementFromTarget cam
  let dist = L.norm disp
      rs   = (cam ^. cameraPreferredDistance) - dist
  t <- liftIO $ P.coAllocateWorldTransform =<< cam ^. cameraController . to P.getGhostObject
  (x, y, z) <- liftIO $ P.getOrigin t
  liftIO $ P.del t
  AL.listenerPosition $= AL.Vertex3 x y z
  setCameraRadialSpeed cam rs
  cameraLookAtTarget cam

turnPlayer :: GameWire s a ()
turnPlayer = mkMConst $ do
  cam <- use gameStateCamera
  target <- use $ gameStatePlayer
  orientation <- getCameraOrientation cam
  setPlayerOrientation target orientation

rotateCamera :: MonadIO m => (Float, Float) -> Camera -> m ()
rotateCamera (dhor, dver) cam = do
  costheta <- getCameraInclinationCos cam
  setCameraPolarSpeed cam (clampy costheta)
  setCameraAzimuthalSpeed cam (CFloat dhor)
  where
    clampy theta
      | theta > 0.9    = CFloat $ max 0 dver
      | theta < (-0.9) = CFloat $ min 0 dver
      | otherwise      = CFloat dver

camera :: GameWire s a ()
camera = (mkConst (Right ()) >>> N.cursorMode N.CursorMode'Reset)
         --> (rotCam <<< N.mouseMickies)
  where
    rotCam = mkGen_ $ \(x, y) -> Right <$> do
      cam <- use gameStateCamera
      rotateCamera (-x * 10, -y * 10)  cam

movePlayer :: GameWire s (L.V3 CFloat) ()
movePlayer = mkGen_ $ \dir -> Right <$> do
--  dir <- getCameraDisplacementFromTarget
  p <- use gameStatePlayer
  setPlayerLinearVelocity p dir

playerHorizontalMovement :: GameWire s a (L.V3 CFloat)
playerHorizontalMovement = (\v -> 0.1 * recip (L.norm v) L.*^ v) <$> solderWire (+) zwire xwire
  where
    fwd = mkMConst $ join $ Lens.uses gameStateCamera getCameraForward
    lft = mkMConst $ join $ Lens.uses gameStateCamera getCameraLeft
    xwire = xorWire (keyA >>> lft) (keyD >>> (negate <$> lft))
    zwire = xorWire (keyW >>> fwd) (keyS >>> (negate <$> fwd))

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
