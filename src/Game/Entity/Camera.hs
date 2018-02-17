module Game.Entity.Camera where

import ClassyPrelude
import Game.Types
import qualified Linear as L
import qualified Physics.Bullet as P
import Control.Lens

-- | The CollisionObject passed to this function is not freed upon
-- destruciton of the Camera.

-- newCamera :: (MonadIO m, P.IsCollisionObject co) => co -> m Camera
-- newCamera co

-- destroyCamera :: MonadIO m => Camera -> m ()
-- destroyCamera cam

-- getCameraLinearVelocity :: MonadIO m => Camera -> m (L.V3 CFloat)
-- getCameraLinearVelocity cam

-- setCameraLinearVelocity :: MonadIO m => Camera -> L.V3 CFloat -> m ()
-- setCameraLinearVelocity cam v

-- getCameraDisplacementToTarget :: MonadIO m => Camera -> m (L.V3 CFloat)

-- getCameraDirectionToTarget :: MonadIO m => Camera -> m (L.V3 CFloat)

-- getCameraRadialSpeed :: MonadIO m => Camera -> m CFloat

-- setCameraRadialSpeed :: MonadIO m => Camera -> CFloat -> m ()

-- | Split like this so we don't have to choose between physics or
-- math language for polar/azimuthal angles.

-- getCameraPolarSpeed :: MonadIO m => Camera -> m CFloat

-- setCameraPolarSpeed :: MonadIO m => Camera -> CFloat -> m ()

-- getCameraAzimuthalSpeed :: MonadIO m => Camera -> m CFloat

-- setCameraAzimuthalSpeed :: MonadIO m => Camera -> CFloat m ()

-- | Useful after switching targets.

-- cameraLookAtTarget :: MonadIO m => Camera -> m ()

