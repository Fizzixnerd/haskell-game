{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Entity.GiantFeaturelessPlane where

import ClassyPrelude
import Game.Types
import Foreign.C.Types
import qualified Physics.Bullet as P
import qualified Linear as L

newGiantFeaturelessPlane :: MonadIO m => L.V3 CFloat -> CFloat -> m (GiantFeaturelessPlane s)
newGiantFeaturelessPlane (L.V3 x y z) pc = liftIO $ do
  gfpShape :: P.StaticPlaneShape <- P.new ((0, 1, 0), pc)
  gfpXform <- P.new ((0,0,0,1), (x, y, z))
  gfpMotionState <- P.new gfpXform
  gfpRigidBodyCI <- P.newRigidBodyConstructionInfo 0 gfpMotionState gfpShape 0 0 0
  gfpRigidBody <- P.newRigidBody gfpRigidBodyCI
  P.del gfpXform
  P.del gfpMotionState
  P.del gfpRigidBodyCI
  let e = Entity
          { _entityChildren = empty
          , _entityGraphics = Nothing
          , _entitySounds   = Nothing
          , _entityLogic    = Nothing
          , _entityRigidBody = Nothing
          , _entityCollisionObject = CollisionObject (P.toCollisionObject gfpRigidBody)
          }
  return GiantFeaturelessPlane
    { _giantFeaturelessPlaneRigidBody = gfpRigidBody
    , _giantFeaturelessPlaneEntity = e
    }

destroyGiantFeaturelessPlane :: MonadIO m => GiantFeaturelessPlane s -> m ()
destroyGiantFeaturelessPlane (GiantFeaturelessPlane gfp _) = liftIO $ P.freeRigidBody gfp
