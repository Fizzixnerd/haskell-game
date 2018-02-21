{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Entity.GiantFeaturelessPlane where

import ClassyPrelude
import Game.Types
import Foreign.C.Types
import qualified Physics.Bullet as P
import qualified Linear as L

newGiantFeaturelessPlane :: MonadIO m => L.V3 CFloat -> CFloat -> m GiantFeaturelessPlane
newGiantFeaturelessPlane (L.V3 x y z) pc = liftIO $ GiantFeaturelessPlane <$> do
  gfpShape :: P.StaticPlaneShape <- P.new ((0, 1, 0), pc)
  gfpXform <- P.new ((0,0,0,1), (x, y, z))
  gfpMotionState <- P.new gfpXform
  gfpRigidBodyCI <- P.newRigidBodyConstructionInfo 0 gfpMotionState gfpShape 0 0 0
  gfpRigidBody <- P.newRigidBody gfpRigidBodyCI
  P.del gfpXform
  P.del gfpMotionState
  P.del gfpRigidBodyCI
  return gfpRigidBody

destroyGiantFeaturelessPlane :: MonadIO m => GiantFeaturelessPlane -> m ()
destroyGiantFeaturelessPlane (GiantFeaturelessPlane gfp) = liftIO $ P.freeRigidBody gfp
