{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Arrows #-}

module Game.Wires where

import Game.Types
import ClassyPrelude
import Foreign.C.Types
import Control.Wire
import Control.Lens
import Game.Entity.Camera
import qualified Linear as L
import qualified Physics.Bullet as P

-- | Is the identity if only one wire is producing. If both are, it
-- merges the results with merge. This steps both wires.
solderWire :: (Monoid e, Monad m) => (b -> b -> b) -> Wire s e m a b -> Wire s e m a b -> Wire s e m a b
solderWire merge' w1 w2 = WGen $ \s eea -> do
  (eeb1, _) <- stepWire w1 s eea
  (eeb2, _) <- stepWire w2 s eea
  let res = merger eeb1 eeb2
  res `seq` return (res, solderWire merge' w1 w2)
  where
    merger = \case
      Left err -> left (mappend err)
      Right x  -> const (Right x) ||| (Right . merge' x)

-- | If both are producing or both are inhibited, then it inhibits.
-- Otherwise it acts like the producing one. This thing has to step
-- both wires.
solderWireM :: (Monoid e, Monad m) => (b -> b -> m b) -> Wire s e m a b -> Wire s e m a b -> Wire s e m a b
solderWireM merge' w1 w2 = WGen $ \s eea -> do
  (eeb1, _) <- stepWire w1 s eea
  (eeb2, _) <- stepWire w2 s eea
  res <- merger eeb1 eeb2
  res `seq` return (res, solderWireM merge' w1 w2)
  where
    merger = \case
      Left err -> pure . left (mappend err)
      Right x  -> const (pure . Right $ x) ||| (fmap Right . merge' x)

-- | An identity wire that executes a monadic action when activated,
-- and passes the input through unchanged
effectiveWire :: Monad m => m c -> Wire s e m a a
effectiveWire act = mkGen_ $ \a -> void act >> return (Right a)

-- | A constant wire that executes a monadic action to obtain its output.
mkConstM :: Monad m => m b -> Wire s e m a b
mkConstM act = mkGen_ $ const $ Right <$> act

-- | A wire transformer that executes the given wire, discards the result, and passes its input through unchanged.
passWire :: Monad m => Wire s e m a b -> Wire s e m a a
passWire wire = (wire &&& id) >>> arr snd

-- Example usage for caching:
-- make a thingUpdateWire that when pulsed will return thing.
-- then feed that into steppingWire
-- Also, to future Matt: past Christian sends his regards.

steppingWire :: Wire s e m a a
steppingWire = mkPureN $ \a -> (Right a, mkConst (Right a))

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

stateIOWire :: MonadIO m => IO b -> Wire s e m a b
stateIOWire act = mkConstM (liftIO act) >>> steppingWire

data CameraState = CameraState
  { _cameraStateLinearVelocity :: L.V3 CFloat
  , _cameraStatePosition       :: L.V3 CFloat
  , _cameraStateTargetPosition :: L.V3 CFloat
  , _cameraStateOrientation    :: L.Quaternion CFloat
  } deriving (Eq, Ord, Show)

-- | Caches camera state when stepped.

cameraStateWire :: MonadIO m => Camera -> Wire s e m a CameraState
cameraStateWire cam = stateIOWire $ do
  vel <- getCameraLinearVelocity cam
  withCameraTransform cam $ \ct -> withTargetTransform cam $ \tt -> do
    camPos  <- bulletV3ToL <$> P.getOrigin ct
    targPos <- bulletV3ToL <$> P.getOrigin tt
    orient  <- bulletQuatToL <$> P.getRotation ct
    return $ CameraState vel camPos targPos orient

cameraDisplacementFromTarget :: Wire s e m CameraState (L.V3 CFloat)
cameraDisplacementFromTarget = mkPure_ $ \CameraState {..} -> Right $ _cameraStatePosition - _cameraStateTargetPosition

cameraForward :: Monad m => Wire s e m CameraState (L.V3 CFloat)
cameraForward = negate . set L._y 0 <$> cameraDisplacementFromTarget

cameraLeft :: Monad m => Wire s e m CameraState (L.V3 CFloat)
cameraLeft = over L._xz L.perp <$> cameraForward

cameraRHat :: Monad m => Wire s e m CameraState (L.V3 CFloat)
cameraRHat = L.normalize <$> cameraDisplacementFromTarget

cameraRadialSpeed :: Monad m => Wire s e m CameraState CFloat
cameraRadialSpeed = proc x -> do
  rh <- cameraForward -< x
  returnA -< L.dot rh (_cameraStateLinearVelocity x)

cameraThetaHat :: Monad m => Wire s e m CameraState (L.V3 CFloat)
cameraThetaHat = proc cs -> do
  v@(L.V3 x y z) <- cameraDisplacementFromTarget -< cs
  let r        = L.norm v
      littleR  = L.norm $ L.V2 x z
      cosTheta = y / r
      sinTheta = littleR / r
      cosPhi   = x / littleR
      sinPhi   = z / littleR
  returnA -< L.V3 (cosTheta * cosPhi) (-sinTheta) (cosTheta * sinPhi)

-- Inward!
cameraInclinationCos :: Monad m => Wire s e m CameraState CFloat
cameraInclinationCos = cameraDisplacementFromTarget >>> arr ((view L._y &&& arr L.norm) >>> (\(y,r) -> negate y / r))

cameraPolarSpeed :: Monad m => Wire s e m CameraState CFloat
cameraPolarSpeed = uncurry L.dot <$> (arr _cameraStateLinearVelocity &&& cameraThetaHat)

cameraPhiHat :: Monad m => Wire s e m CameraState (L.V3 CFloat)
cameraPhiHat = cameraDisplacementFromTarget <&> \(L.V3 x _ y) ->
  let littleR = L.norm (L.V2 x y)
  in  L.V3 (- y / littleR) 0 (x / littleR)

cameraAzimuthalSpeed :: Monad m => Wire s e m CameraState CFloat
cameraAzimuthalSpeed = uncurry L.dot <$> (arr _cameraStateLinearVelocity &&& cameraPhiHat)

cameraV' :: Monad m => Wire s e m CameraState VMatrix
cameraV' = arr $ \CameraState {..} ->
  let pos = _cameraStatePosition
      tar = _cameraStateTargetPosition
      vup = L.V3 0 1 0
  in fmap (\(CFloat x) -> x) <$> L.lookAt pos tar vup

cameraVP' :: Monad m => Wire s e m CameraState VPMatrix
cameraVP' = proc cam -> do
  pos <- arr _cameraStatePosition       -< cam
  tar <- arr _cameraStateTargetPosition -< cam
  let camView = L.lookAt pos tar vup
      vup = L.V3 0 1 0
      cfov = 60 -- fix -_-
      camPerspective = L.perspective cfov (16/9) 0.1 100
    in
    returnA -< camPerspective L.!*! fmap (fmap (\(CFloat x) -> x)) camView
