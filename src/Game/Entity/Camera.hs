{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Entity.Camera where

import ClassyPrelude
import Game.Types
import Foreign.C.Types
import Game.Entity
import qualified Linear as L
import qualified Physics.Bullet as P
import Control.Lens

-- | The CollisionObject passed to this function is not freed upon
-- destruction of the Camera.
newCamera :: (MonadIO m, P.IsCollisionObject co) => co -> Float -> m (Camera s)
newCamera target _cameraPreferredDistance' = liftIO $ do
  let _cameraTarget = P.toCollisionObject target
      _cameraPreferredDistance = CFloat _cameraPreferredDistance'
  -- pcgo :: P.PairCachingGhostObject <- P.new ()
  startXform <- P.new ((0, 0, 0, 0), (0, 0, 0))
  P.setIdentity startXform
  P.setOrigin startXform 0 0 0
  cameraMotionState <- P.new startXform
  cameraShape <- P.newSphereShape 0.5
  rbci <- P.newRigidBodyConstructionInfo 1 cameraMotionState cameraShape 0 0 0
  _cameraController <- P.newRigidBody rbci
  P.setActivationState _cameraController P.activationStateDisableDeactivation
  P.del startXform
  P.del rbci

  let _cameraFOV = pi/3
      _cameraEntity = Entity
                      { _entityChildren = empty
                      , _entityGraphics = Nothing
                      , _entitySounds   = Nothing
                      , _entityLogic    = Nothing
                      , _entityRigidBody = Just $ RigidBody _cameraController
                      , _entityCollisionObject = CollisionObject (P.toCollisionObject _cameraController)
                      }
  return Camera {..}

destroyCamera :: MonadIO m => Camera s -> m ()
destroyCamera Camera {..} = liftIO $ P.freeRigidBody _cameraController

getCameraLinearVelocity :: MonadIO m => Camera s -> m (L.V3 Float)
getCameraLinearVelocity Camera {..} = getEntityLinearVelocity _cameraEntity

getCameraLinearForce :: MonadIO m => Camera s -> m (L.V3 Float)
getCameraLinearForce Camera {..} = getEntityForce _cameraEntity

setCameraLinearForce :: MonadIO m => Camera s -> L.V3 Float -> m ()
setCameraLinearForce Camera {..} = setEntityForce _cameraEntity

setCameraLinearVelocity :: MonadIO m => Camera s -> L.V3 Float -> m ()
setCameraLinearVelocity Camera {..} = setEntityLinearVelocity _cameraEntity

allocateCameraTransform :: MonadIO m => Camera s -> m P.Transform
allocateCameraTransform c = liftIO $ P.coAllocateWorldTransform (c ^. cameraController)

allocateTargetTransform :: MonadIO m => Camera s -> m P.Transform
allocateTargetTransform c = liftIO $ P.coAllocateWorldTransform (c ^. cameraTarget)

withCameraTransform :: MonadIO m => Camera s -> (P.Transform -> IO b) -> m b
withCameraTransform c f = liftIO $ bracket (allocateCameraTransform c) P.del f

withTargetTransform :: MonadIO m => Camera s -> (P.Transform -> IO b) -> m b
withTargetTransform c f = liftIO $ bracket (allocateTargetTransform c) P.del f

getCameraPosition :: MonadIO m => Camera s -> m (L.V3 Float)
getCameraPosition Camera {..} = getEntityPosition _cameraEntity

getCameraTargetPosition :: MonadIO m => Camera s -> m (L.V3 Float)
getCameraTargetPosition c = withTargetTransform c $ \t -> do
  (CFloat x, CFloat y, CFloat z) <- P.getOrigin t
  return $ L.V3 x y z

getCameraOrientation :: MonadIO m => Camera s -> m (L.Quaternion Float)
getCameraOrientation Camera {..} = getEntityOrientation _cameraEntity

getCameraDisplacementFromTarget :: MonadIO m => Camera s -> m (L.V3 Float)
getCameraDisplacementFromTarget cam = do
  targetPos <- getCameraTargetPosition cam
  camPos <- getCameraPosition cam
  return $ camPos - targetPos

-- Not normalized.
getCameraForward :: MonadIO m => Camera s -> m (L.V3 Float)
getCameraForward = fmap (negate . set L._y 0) . getCameraDisplacementFromTarget

getCameraLeft :: MonadIO m => Camera s -> m (L.V3 Float)
getCameraLeft = fmap (over L._xz L.perp) . getCameraForward

getCameraRHat :: MonadIO m => Camera s -> m (L.V3 Float)
getCameraRHat = fmap L.normalize <$> getCameraDisplacementFromTarget

-- r, theta, phi; theta is polar, phi is azimuthal
-- | Radial unit vector points OUT (i.e. points to BEHIND the Camera).

-- from Wikipedia: https://en.wikipedia.org/wiki/Spherical_coordinate_system
--
-- r = {\sqrt {x^{2}+y^{2}+z^{2}}}
-- theta = \arccos {\frac {z}{\sqrt {x^{2}+y^{2}+z^{2}}}}=\arccos {\frac {z}{r}}
-- varphi = \arctan {\frac {y}{x}}
--
-- \hat {\mathbf {r} }} = \sin \theta \cos \varphi \,{\hat {\mathbf {x} }}+\sin \theta \sin \varphi \,{\hat {\mathbf {y} }}+\cos \theta \,{\hat {\mathbf {z} }}
-- {\hat {\boldsymbol {\theta }}} = \cos \theta \cos \varphi \,{\hat {\mathbf {x} }}+\cos \theta \sin \varphi \,{\hat {\mathbf {y} }}-\sin \theta \,{\hat {\mathbf {z} }}
-- {\hat {\boldsymbol {\varphi }}} = -\sin \varphi \,{\hat {\mathbf {x} }}+\cos \varphi \,{\hat {\mathbf {y} }}\end{aligned}}}

getCameraRadialForce :: MonadIO m => Camera s -> m Float
getCameraRadialForce cam = do
  rhat <- getCameraRHat cam
  f <- getCameraLinearForce cam
  return $ L.dot rhat f

getCameraRadialSpeed :: MonadIO m => Camera s -> m Float
getCameraRadialSpeed cam = do
  rhat <- getCameraRHat cam
  v <- getCameraLinearVelocity cam
  return $ L.dot rhat v

setCameraRadialForce :: MonadIO m => Camera s -> Float -> m ()
setCameraRadialForce cam rf = do
  rfOld <- getCameraRadialForce cam
  rhat <- getCameraRHat cam
  f <- getCameraLinearForce cam
  setCameraLinearForce cam (f + ((rf - rfOld) L.*^ rhat))

setCameraTransform :: MonadIO m => Camera s -> P.Transform -> m ()
setCameraTransform c t = liftIO $ P.coSetWorldTransform (c ^. cameraController) t

-- | Polar unit vector points DOWN.

-- This is z-up!
-- theta = arccos z / r

-- phi = arctan y / x

-- sin theta = littleR / r
-- cos theta = z / r

-- cos phi = x / littleR
-- sin phi = y / littleR

getCameraThetaHat :: MonadIO m => Camera s -> m (L.V3 Float)
getCameraThetaHat cam = do
  v@(L.V3 x y z) <- getCameraDisplacementFromTarget cam
  let r        = L.norm v
      littleR  = L.norm $ L.V2 x z
      cosTheta = y / r
      sinTheta = littleR / r
      cosPhi   = x / littleR
      sinPhi   = z / littleR
  return $ L.V3 (cosTheta * cosPhi) (-sinTheta) (cosTheta * sinPhi)

-- i.e. arccos of this is the inclination
-- this is INWARD
getCameraInclinationCos :: MonadIO m => Camera s -> m Float
getCameraInclinationCos cam = do
  v@(L.V3 _ y _) <- getCameraDisplacementFromTarget cam
  let r = L.norm v
  return $ negate y / r

getCameraPolarForce :: MonadIO m => Camera s -> m Float
getCameraPolarForce cam = do
  f <- getCameraLinearForce cam
  thetahat <- getCameraThetaHat cam
  return $ L.dot thetahat f

getCameraPolarSpeed :: MonadIO m => Camera s -> m Float
getCameraPolarSpeed cam = do
  v <- getCameraLinearVelocity cam
  thetahat <- getCameraThetaHat cam
  return $ L.dot thetahat v

setCameraPolarForce :: MonadIO m => Camera s -> Float -> m ()
setCameraPolarForce cam pf = do
  pfOld <- getCameraPolarForce cam
  thetahat <- getCameraThetaHat cam
  f <- getCameraLinearForce cam
  cameraApplyForce cam (f + ((pf - pfOld) L.*^ thetahat))

getCameraPhiHat :: MonadIO m => Camera s -> m (L.V3 Float)
getCameraPhiHat cam = do
  (L.V3 x _ y) <- getCameraDisplacementFromTarget cam
  let littleR = L.norm (L.V2 x y)
      phihat = L.V3 (- y / littleR) 0 (x / littleR)
  return phihat

getCameraAzimuthalForce :: MonadIO m => Camera s -> m Float
getCameraAzimuthalForce cam = do
  f <- getCameraLinearForce cam
  phihat <- getCameraPhiHat cam
  return $ L.dot phihat f

getCameraAzimuthalSpeed :: MonadIO m => Camera s -> m Float
getCameraAzimuthalSpeed cam = do
  v <- getCameraLinearVelocity cam
  phihat <- getCameraPhiHat cam
  return $ L.dot phihat v

setCameraAzimuthalForce :: MonadIO m => Camera s -> Float -> m ()
setCameraAzimuthalForce cam af = do
  afOld <- getCameraAzimuthalForce cam
  phihat <- getCameraPhiHat cam
  f <- getCameraLinearForce cam
  cameraApplyForce cam (f + ((af - afOld) L.*^ phihat))

cameraApplyAzimuthalForce :: MonadIO m => Camera s -> Float -> m ()
cameraApplyAzimuthalForce cam af = do
  phiHat <- getCameraPhiHat cam
  cameraApplyForce cam (af L.*^ phiHat)

cameraAttach :: (MonadIO m, P.IsCollisionObject co) => Camera s -> co -> m (Camera s)
cameraAttach cam co = return $ cam & cameraTarget .~ P.toCollisionObject co

cameraApplyForce :: MonadIO m => Camera s -> L.V3 Float -> m ()
cameraApplyForce cam v = liftIO $ P.applyForce (cam ^. cameraController)
                         (CFloat $ v ^. L._x)
                         (CFloat $ v ^. L._y)
                         (CFloat $ v ^. L._z)
                         0
                         0
                         0

cameraApplyTorque :: MonadIO m => Camera s -> L.V3 Float -> m ()
cameraApplyTorque cam omega = liftIO $ P.applyTorque (cam ^. cameraController)
                              (CFloat $ omega ^. L._x)
                              (CFloat $ omega ^. L._y)
                              (CFloat $ omega ^. L._z)

cameraVP :: MonadIO m => Camera s -> m VPMatrix
cameraVP cam = do
  pos <- getCameraPosition cam
  tar <- getCameraTargetPosition cam
  let camView = L.lookAt pos tar vup
  return $ (cam ^. to cameraP)  L.!*! camView
    where
      vup = L.V3 0 1 0

cameraV :: MonadIO m => Camera s -> m VMatrix
cameraV cam = do
  pos <- getCameraPosition cam
  tar <- getCameraTargetPosition cam
  let vup = L.V3 0 1 0
  return $ L.lookAt pos tar vup

cameraP :: Camera s -> PMatrix
cameraP cam = L.perspective cfov (16/9) 0.1 100
  where cfov = cam ^. cameraFOV
