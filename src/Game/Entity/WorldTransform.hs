{-# LANGUAGE NoImplicitPrelude #-}

module Game.Entity.WorldTransform where

import ClassyPrelude
import Control.Lens
import Foreign.C.Types
import Game.Types
import qualified Linear as L
import qualified Physics.Bullet as P

getWorldMatrix :: MonadIO m => CollisionObject -> m (L.M44 Float)
getWorldMatrix wt =
  liftIO $ bracket (P.coAllocateWorldTransform $ wt ^. unCollisionObject) P.del $
    \t -> do
      (CFloat i, CFloat j, CFloat k, CFloat r) <- P.getRotation t
      (CFloat x, CFloat y, CFloat z) <-  P.getOrigin t
      return $ L.mkTransformation (L.Quaternion r (L.V3 i j k)) (L.V3 x y z)

getWorldPosition :: MonadIO m => CollisionObject -> m (L.V3 Float)
getWorldPosition wt =
  liftIO $ bracket (P.coAllocateWorldTransform $ wt ^. unCollisionObject) P.del $
    \t -> do
      (CFloat x, CFloat y, CFloat z) <- P.getOrigin t
      return $ L.V3 x y z

