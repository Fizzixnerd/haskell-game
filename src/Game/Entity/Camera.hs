{-# LANGUAGE RecordWildCards #-}

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
  _cameraController <- P.newCollisionObject
  return Camera {..}

destroyCamera :: MonadIO m => Camera -> m ()
destroyCamera Camera {..} = liftIO $ P.del _cameraController

getCameraLinearVelocity :: MonadIO m => Camera -> m (L.V3 CFloat)
getCameraLinearVelocity Camera {..} = liftIO $ do
  (x, y, z) <- P.getInterpolationLinearVelocity _cameraController
  return $ L.V3 x y z

setCameraLinearVelocity :: MonadIO m => L.V3 CFloat ->  Camera -> m ()
setCameraLinearVelocity (L.V3 x y z) Camera {..} =
  liftIO $ P.setInterpolationLinearVelocity _cameraController x y z

-- updateCameraLinearVelocity :: (L.V3 CFloat -> IO (L.V3 CFloat)) -> Camera -> IO ()
-- updateCameraLinearVelocity f cam = do
--   v <- liftIO $ getCameraLinearVelocity cam
--   v' <- f v
--   liftIO $ setCameraLinearVelocity v' cam 

-- cameraLinearVelocity = reference getCameraLinearVelocity setCameraLinearVelocity updateCameraLinearVelocity

getCameraPosition :: MonadIO m => Camera -> m (L.V3 CFloat)
getCameraPosition Camera {..} = liftIO $ do
  t <- P.coAllocateWorldTransform _cameraController
  (x, y, z) <- P.getOrigin t
  P.del t
  return $ L.V3 x y z

getCameraTargetPosition :: MonadIO m => Camera -> m (L.V3 CFloat)
getCameraTargetPosition Camera {..} = liftIO $ do
  t <- P.coAllocateWorldTransform _cameraTarget
  (x, y, z) <- P.getOrigin t
  P.del t
  return $ L.V3 x y z

getCameraOrientation :: MonadIO m => Camera -> m (L.Quaternion CFloat)
getCameraOrientation Camera {..} = liftIO $ do
  t <- P.coAllocateWorldTransform _cameraController
  (i, j, k, r) <- P.getRotation t
  return $ L.Quaternion r (L.V3 i j k)

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

-- | Polar unit vector points DOWN.

getCameraThetaHat :: MonadIO m => Camera -> m (L.V3 CFloat)
getCameraThetaHat cam = do
  rhat <- getCameraRHat cam
  phihat <- getCameraPhiHat cam
  return $ L.cross phihat rhat

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
  (L.V3 x y _) <- getCameraDisplacementFromTarget cam
  let littleR = L.norm (L.V2 x y)
      phihat = L.V3 (- y / littleR) (x / littleR) 0
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

-- cameraLookAtTarget :: MonadIO m => Camera -> m ()
-- cameraLookAtTarget cam = do
--   rhat <- getCameraRHat cam
--   let L.V3 x y z = negate rhat
--       controller = cam ^. cameraController
--   t <- P.coAllocateWorldTransform controller
  
--   liftIO $ P.setRotation controller x y z 0

-- cameraAttach :: (MonadIO m, P.IsCollisionObject co) => Camera -> co -> m ()
