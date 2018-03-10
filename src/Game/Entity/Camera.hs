{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Entity.Camera where

import ClassyPrelude
import Game.Types
import Game.Graphics.Types
import Foreign.C.Types
import qualified Linear as L
import qualified Physics.Bullet as P
import Control.Lens

-- | The CollisionObject passed to this function is not freed upon
-- destruction of the Camera.
newCamera :: (MonadIO m, P.IsCollisionObject co) => co -> CFloat -> m (Camera s)
newCamera target _cameraPreferredDistance = liftIO $ do
  let _cameraTarget = P.toCollisionObject target
  -- pcgo :: P.PairCachingGhostObject <- P.new ()
  startXform <- P.new ((0, 0, 0, 0), (0, 0, 0))
  P.setIdentity startXform
  P.setOrigin startXform 0 0 0
  cameraMotionState <- P.new startXform
  cameraShape <- P.newSphereShape 0.5
  rbci <- P.newRigidBodyConstructionInfo 1 cameraMotionState cameraShape 0 0 0
  _cameraController <- P.newRigidBody rbci
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

bulletV3ToL :: (a,a,a) -> L.V3 a
bulletV3ToL (x,y,z) = L.V3 x y z

bulletV4ToL :: (a,a,a,a) -> L.V4 a
bulletV4ToL (x,y,z,w) = L.V4 x y z w

bulletQuatToL :: (a,a,a,a) -> L.Quaternion a
bulletQuatToL (i,j,k,r) = L.Quaternion r (L.V3 i j k)

getCameraLinearVelocity :: MonadIO m => Camera s -> m (L.V3 CFloat)
getCameraLinearVelocity Camera {..} = liftIO $ bulletV3ToL <$> P.rbGetLinearVelocity _cameraController

getCameraLinearForce :: MonadIO m => Camera s -> m (L.V3 CFloat)
getCameraLinearForce Camera {..} = liftIO $ bulletV3ToL <$> P.getTotalForce _cameraController

setCameraLinearForce :: MonadIO m => Camera s -> L.V3 CFloat -> m ()
setCameraLinearForce cam f = liftIO $ do
  P.clearForces $ cam ^. cameraController
  cameraApplyForce cam f

setCameraLinearVelocity :: MonadIO m => L.V3 CFloat -> Camera s -> m ()
setCameraLinearVelocity (L.V3 x y z) Camera {..} =
  liftIO $ P.rbSetLinearVelocity _cameraController x y z

allocateCameraTransform :: MonadIO m => Camera s -> m P.Transform
allocateCameraTransform c = liftIO $ P.coAllocateWorldTransform (c ^. cameraController)

allocateTargetTransform :: MonadIO m => Camera s -> m P.Transform
allocateTargetTransform c = liftIO $ P.coAllocateWorldTransform (c ^. cameraTarget)

withCameraTransform :: MonadIO m => Camera s -> (P.Transform -> IO b) -> m b
withCameraTransform c f = liftIO $ bracket (allocateCameraTransform c) P.del f

withTargetTransform :: MonadIO m => Camera s -> (P.Transform -> IO b) -> m b
withTargetTransform c f = liftIO $ bracket (allocateTargetTransform c) P.del f

getCameraPosition :: MonadIO m => Camera s -> m (L.V3 CFloat)
getCameraPosition c = liftIO $ withCameraTransform c (\t -> do
                                                         (x, y, z) <- P.getOrigin t
                                                         return $ L.V3 x y z)

getCameraTargetPosition :: MonadIO m => Camera s -> m (L.V3 CFloat)
getCameraTargetPosition c = liftIO $ withTargetTransform c
                            (\t -> do
                                (x, y, z) <- P.getOrigin t
                                return $ L.V3 x y z)

getCameraOrientation :: MonadIO m => Camera s -> m (L.Quaternion CFloat)
getCameraOrientation c = liftIO $ withCameraTransform c
                         (\t -> do
                             (i, j, k, r) <- P.getRotation t
                             return $ L.Quaternion r (L.V3 i j k))

getCameraDisplacementFromTarget :: MonadIO m => Camera s -> m (L.V3 CFloat)
getCameraDisplacementFromTarget cam = do
  targetPos <- getCameraTargetPosition cam
  camPos <- getCameraPosition cam
  return $ camPos - targetPos

-- Not normalized.
getCameraForward :: MonadIO m => Camera s -> m (L.V3 CFloat)
getCameraForward = fmap (negate . set L._y 0) . getCameraDisplacementFromTarget

getCameraLeft :: MonadIO m => Camera s -> m (L.V3 CFloat)
getCameraLeft = fmap (over L._xz L.perp) . getCameraForward

getCameraRHat :: MonadIO m => Camera s -> m (L.V3 CFloat)
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

getCameraRadialForce :: MonadIO m => Camera s -> m CFloat
getCameraRadialForce cam = do
  rhat <- getCameraRHat cam
  f <- getCameraLinearForce cam
  return $ L.dot rhat f

getCameraRadialSpeed :: MonadIO m => Camera s -> m CFloat
getCameraRadialSpeed cam = do
  rhat <- getCameraRHat cam
  v <- getCameraLinearVelocity cam
  return $ L.dot rhat v

setCameraRadialForce :: MonadIO m => Camera s -> CFloat -> m ()
setCameraRadialForce cam rf = do
  rfOld <- getCameraRadialForce cam
  rhat <- getCameraRHat cam
  f <- getCameraLinearForce cam
  setCameraLinearForce cam (f + ((rf - rfOld) L.*^ rhat))

setCameraTransform :: MonadIO m => Camera s -> P.Transform -> m ()
setCameraTransform c t = liftIO $ do
  P.coSetWorldTransform (c ^. cameraController) t

-- | Polar unit vector points DOWN.

-- This is z-up!
-- theta = arccos z / r

-- phi = arctan y / x

-- sin theta = littleR / r
-- cos theta = z / r

-- cos phi = x / littleR
-- sin phi = y / littleR

getCameraThetaHat :: MonadIO m => Camera s -> m (L.V3 CFloat)
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
getCameraInclinationCos :: MonadIO m => Camera s -> m CFloat
getCameraInclinationCos cam = do
  v@(L.V3 _ y _) <- getCameraDisplacementFromTarget cam
  let r = L.norm v
  return $ negate y / r

getCameraPolarForce :: MonadIO m => Camera s -> m CFloat
getCameraPolarForce cam = do
  f <- getCameraLinearForce cam
  thetahat <- getCameraThetaHat cam
  return $ L.dot thetahat f

getCameraPolarSpeed :: MonadIO m => Camera s -> m CFloat
getCameraPolarSpeed cam = do
  v <- getCameraLinearVelocity cam
  thetahat <- getCameraThetaHat cam
  return $ L.dot thetahat v

setCameraPolarForce :: MonadIO m => Camera s -> CFloat -> m ()
setCameraPolarForce cam pf = do
  pfOld <- getCameraPolarForce cam
  thetahat <- getCameraThetaHat cam
  f <- getCameraLinearForce cam
  cameraApplyForce cam (f + ((pf - pfOld) L.*^ thetahat))

getCameraPhiHat :: MonadIO m => Camera s -> m (L.V3 CFloat)
getCameraPhiHat cam = do
  (L.V3 x _ y) <- getCameraDisplacementFromTarget cam
  let littleR = L.norm (L.V2 x y)
      phihat = L.V3 (- y / littleR) 0 (x / littleR)
  return phihat

getCameraAzimuthalForce :: MonadIO m => Camera s -> m CFloat
getCameraAzimuthalForce cam = do
  f <- getCameraLinearForce cam
  phihat <- getCameraPhiHat cam
  return $ L.dot phihat f

getCameraAzimuthalSpeed :: MonadIO m => Camera s -> m CFloat
getCameraAzimuthalSpeed cam = do
  v <- getCameraLinearVelocity cam
  phihat <- getCameraPhiHat cam
  return $ L.dot phihat v

setCameraAzimuthalForce :: MonadIO m => Camera s -> CFloat -> m ()
setCameraAzimuthalForce cam af = do
  afOld <- getCameraAzimuthalForce cam
  phihat <- getCameraPhiHat cam
  f <- getCameraLinearForce cam
  cameraApplyForce cam (f + ((af - afOld) L.*^ phihat))

cameraApplyAzimuthalForce :: MonadIO m => Camera s -> CFloat -> m ()
cameraApplyAzimuthalForce cam af = do
  phiHat <- getCameraPhiHat cam
  cameraApplyForce cam (af L.*^ phiHat)

cameraAttach :: (MonadIO m, P.IsCollisionObject co) => Camera s -> co -> m (Camera s)
cameraAttach cam co = return $ cam & cameraTarget .~ P.toCollisionObject co

cameraApplyForce :: MonadIO m => Camera s -> L.V3 CFloat -> m ()
cameraApplyForce cam v = liftIO $ P.applyForce (cam ^. cameraController)
                         (v ^. L._x)
                         (v ^. L._y)
                         (v ^. L._z)
                         0
                         0
                         0

cameraApplyTorque :: MonadIO m => Camera s -> L.V3 CFloat -> m ()
cameraApplyTorque cam omega = liftIO $ P.applyTorque (cam ^. cameraController)
                              (omega ^. L._x)
                              (omega ^. L._y)
                              (omega ^. L._z)

cameraVP :: MonadIO m => Camera s -> m VPMatrix
cameraVP cam = do
  pos <- getCameraPosition cam
  tar <- getCameraTargetPosition cam
  let camView = L.lookAt pos tar vup
  return $ (cam ^. to cameraP)  L.!*! fmap (fmap (\(CFloat x) -> x)) camView
    where
      vup = L.V3 0 1 0

cameraV :: MonadIO m => Camera s -> m VMatrix
cameraV cam = do
  pos <- getCameraPosition cam
  tar <- getCameraTargetPosition cam
  let vup = L.V3 0 1 0
  return $ fmap (\(CFloat x) -> x) <$> L.lookAt pos tar vup

cameraP :: Camera s -> PMatrix
cameraP cam = L.perspective cfov (16/9) 0.1 100
  where cfov = cam ^. cameraFOV
