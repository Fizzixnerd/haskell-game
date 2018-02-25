{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Entity.Camera where

import ClassyPrelude
import Game.Types
import Foreign.C.Types
import qualified Linear as L
import qualified Physics.Bullet as P
import Control.Lens

-- | The CollisionObject passed to this function is not freed upon
-- destruciton of the Camera.

newCamera :: (MonadIO m, P.IsCollisionObject co) => co -> CFloat -> m Camera
newCamera target _cameraPreferredDistance = liftIO $ do
  let _cameraTarget = P.toCollisionObject target
  pcgo :: P.PairCachingGhostObject <- P.new ()
  startXform <- P.new ((0, 0, 0, 0), (0, 0, 0))
  P.setIdentity startXform
  P.setOrigin startXform 0 0 0
  P.coSetWorldTransform pcgo startXform
  P.del startXform

  cameraShape :: P.SphereShape <- P.new 1
  let stepHeight = 0
  _cameraController <- P.newKinematicCharacterController pcgo cameraShape stepHeight
  P.setCollisionShape pcgo cameraShape
  P.setUp _cameraController 0 1 0
  let _cameraFOV = pi/3
  return Camera {..}

destroyCamera :: MonadIO m => Camera -> m ()
destroyCamera Camera {..} = liftIO $ P.del _cameraController

getCameraLinearVelocity :: MonadIO m => Camera -> m (L.V3 CFloat)
getCameraLinearVelocity Camera {..} = liftIO $ do
  (x, y, z) <- P.getLinearVelocity _cameraController
  return $ L.V3 x y z

setCameraLinearVelocity :: MonadIO m => L.V3 CFloat -> Camera -> m ()
setCameraLinearVelocity (L.V3 x y z) Camera {..} =
  liftIO $ P.setLinearVelocity _cameraController x y z

allocateCameraTransform :: MonadIO m => Camera -> m P.Transform
allocateCameraTransform c = liftIO $
                            P.getGhostObject (c ^. cameraController) >>=
                            P.coAllocateWorldTransform

allocateTargetTransform :: MonadIO m => Camera -> m P.Transform
allocateTargetTransform c = liftIO $ P.coAllocateWorldTransform (c ^. cameraTarget)

withCameraTransform :: MonadIO m => Camera -> (P.Transform -> IO b) -> m b
withCameraTransform c f = liftIO $ bracket (allocateCameraTransform c) P.del f

withTargetTransform :: MonadIO m => Camera -> (P.Transform -> IO b) -> m b
withTargetTransform c f = liftIO $ bracket (allocateTargetTransform c) P.del f

getCameraPosition :: MonadIO m => Camera -> m (L.V3 CFloat)
getCameraPosition c = liftIO $ withCameraTransform c (\t -> do
                                                         (x, y, z) <- P.getOrigin t
                                                         return $ L.V3 x y z)

getCameraTargetPosition :: MonadIO m => Camera -> m (L.V3 CFloat)
getCameraTargetPosition c = liftIO $ withTargetTransform c 
                            (\t -> do
                                (x, y, z) <- P.getOrigin t
                                return $ L.V3 x y z)

getCameraOrientation :: MonadIO m => Camera -> m (L.Quaternion CFloat)
getCameraOrientation c = liftIO $ withCameraTransform c 
                         (\t -> do
                             (i, j, k, r) <- P.getRotation t
                             return $ L.Quaternion r (L.V3 i j k))

getCameraDisplacementFromTarget :: MonadIO m => Camera -> m (L.V3 CFloat)
getCameraDisplacementFromTarget cam = do
  targetPos <- getCameraTargetPosition cam
  camPos <- getCameraPosition cam
  return $ camPos - targetPos

getCameraRHat :: MonadIO m => Camera -> m (L.V3 CFloat)
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

getCameraRadialSpeed :: MonadIO m => Camera -> m CFloat
getCameraRadialSpeed cam = do
  rhat <- getCameraRHat cam
  v <- getCameraLinearVelocity cam
  return $ L.dot rhat v

setCameraRadialSpeed :: MonadIO m => Camera -> CFloat -> m ()
setCameraRadialSpeed cam rv = do
  rvOld <- getCameraRadialSpeed cam
  rhat <- getCameraRHat cam
  v <- getCameraLinearVelocity cam
  setCameraLinearVelocity (v + ((rv - rvOld) L.*^ rhat)) cam

setCameraTransform :: MonadIO m => Camera -> P.Transform -> m ()
setCameraTransform c t = liftIO $ do
  go <- P.getGhostObject (c ^. cameraController)
  P.coSetWorldTransform go t

-- | Polar unit vector points DOWN.

-- theta = arccos z / r

-- phi = arctan y / x

-- sin theta = littleR / r
-- cos theta = z / r

-- cos phi = x / littleR
-- sin phi = y / littleR

getCameraThetaHat :: MonadIO m => Camera -> m (L.V3 CFloat)
getCameraThetaHat cam = do
  v@(L.V3 x y z) <- getCameraDisplacementFromTarget cam
  let r        = L.norm v
      littleR  = L.norm $ L.V2 x z
      cosTheta = y / r
      sinTheta = littleR / r
      cosPhi   = x / littleR
      sinPhi   = z / littleR
  return $ L.V3 (cosTheta * cosPhi) (-sinTheta) (cosTheta * sinPhi)

getCameraPolarSpeed :: MonadIO m => Camera -> m CFloat
getCameraPolarSpeed cam = do
  v <- getCameraLinearVelocity cam
  thetahat <- getCameraThetaHat cam
  return $ L.dot thetahat v

setCameraPolarSpeed :: MonadIO m => Camera -> CFloat -> m ()
setCameraPolarSpeed cam pv = do
  pvOld <- getCameraPolarSpeed cam
  thetahat <- getCameraThetaHat cam
  v <- getCameraLinearVelocity cam
  setCameraLinearVelocity (v + ((pv - pvOld) L.*^ thetahat)) cam

getCameraPhiHat :: MonadIO m => Camera -> m (L.V3 CFloat)
getCameraPhiHat cam = do
  (L.V3 x _ y) <- getCameraDisplacementFromTarget cam
  let littleR = L.norm (L.V2 x y)
      phihat = L.V3 (- y / littleR) 0 (x / littleR) 
  return phihat

getCameraAzimuthalSpeed :: MonadIO m => Camera -> m CFloat
getCameraAzimuthalSpeed cam = do
  v <- getCameraLinearVelocity cam
  phihat <- getCameraPhiHat cam
  return $ L.dot phihat v

setCameraAzimuthalSpeed :: MonadIO m => Camera -> CFloat -> m ()
setCameraAzimuthalSpeed cam av = do
  avOld <- getCameraAzimuthalSpeed cam
  phihat <- getCameraPhiHat cam
  v <- getCameraLinearVelocity cam
  setCameraLinearVelocity (v + ((av - avOld) L.*^ phihat)) cam

-- | Useful after switching targets.

cameraLookAtTarget :: MonadIO m => Camera -> m ()
cameraLookAtTarget cam = do
  rhat <- getCameraRHat cam
  let L.V3 x y z = negate rhat
  withCameraTransform cam
    (\t -> do
        P.setRotation t x y z 0
        go <- P.getGhostObject (cam ^. cameraController)
        P.coSetWorldTransform go t)

cameraAttach :: (MonadIO m, P.IsCollisionObject co) => Camera -> co -> m Camera
cameraAttach cam co = return $ cam & cameraTarget .~ (P.toCollisionObject co)

-- getCameraOpenGLMatrix :: MonadIO m => Camera -> m (L.M44 CFloat)
-- getCameraOpenGLMatrix cam = liftIO $ bracket
--                             (P.coAllocateWorldTransform $ cam ^. cameraController)
--                             P.del
--                             P.getOpenGLMatrix

cameraMVP :: MonadIO m => Camera -> m (L.M44 Float)
cameraMVP cam = do
  pos <- getCameraPosition cam
  tar <- getCameraTargetPosition cam
  let camView = L.lookAt pos tar vup

  --  camModel <- getCameraOpenGLMatrix cam
  let camModel = L.identity
  return $ camPerspective L.!*! fmap (fmap (\(CFloat x) -> x)) camView L.!*! camModel
    where
        -- Projection matrix : 90deg Field of View, 16:9 ratio, display range : 0.1 unit <-> 100 units
      vup = L.V3 0 1 0
      cfov = cam ^. cameraFOV
      camPerspective = L.perspective cfov (16/9) 0.1 10000
