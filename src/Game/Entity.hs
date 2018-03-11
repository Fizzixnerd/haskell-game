{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Entity where

import ClassyPrelude
import Game.Entity.WorldTransform
import Game.Types
import Game.Graphics.Types
import Game.Entity.Camera
import Graphics.Binding
-- import Game.Graphics.Shader.Loader
import Game.Graphics.Texture.Loader
import qualified Linear as L
import Control.Lens
import Foreign.C.Types
import qualified Linear as L
import qualified Physics.Bullet as P

entityLocalClosestRayCast :: Entity s
                          -> L.V3 Float -- ^ in Entity-local coordinates
                          -> (Entity s -> Game s ()) -- ^ callback
                          -> Game s ()
entityLocalClosestRayCast e v cb = do
  dw <- use $ gameStatePhysicsWorld . physicsWorldDynamicsWorld
  entityCenter@(L.V3 fromx fromy fromz) <- getWorldPosition $ e ^. entityCollisionObject
  let (L.V3 tox toy toz) = entityCenter + v
  crrc :: P.ClosestRayResultCallback <- liftIO $ P.new ((CFloat fromx, CFloat fromy, CFloat fromz),
                                                        (CFloat tox, CFloat toy, CFloat toz))
  liftIO $ P.rayTest dw (CFloat fromx) (CFloat fromy) (CFloat fromz)
    (CFloat tox) (CFloat toy) (CFloat toz) crrc
  whenM (liftIO . P.rrcHasHit $ crrc) $ do
    co <- liftIO $ P.rrcGetHit crrc
    n <- liftIO $ P.getUserIndex co
    es <- use $ gameStatePhysicsWorld . physicsWorldEntities
    cb $ es `unsafeIndex` n

entityApplyForce :: MonadIO m => Entity s -> L.V3 Float -> m ()
entityApplyForce e (L.V3 x y z) = liftIO $
  case e ^. entityRigidBody of
    Just (RigidBody rb) -> P.applyForce rb (CFloat x) (CFloat y) (CFloat z) 0 0 0
    Nothing -> return ()

entityApplyTorque :: MonadIO m => Entity s -> L.V3 Float -> m ()
entityApplyTorque e (L.V3 x y z) = liftIO $
  case e ^. entityRigidBody of
    Just (RigidBody rb) -> P.applyTorque rb (CFloat x) (CFloat y) (CFloat z)
    Nothing -> return ()

allocateEntityTransform :: MonadIO m => Entity s -> m P.Transform
allocateEntityTransform e = liftIO $
                            P.coAllocateWorldTransform $
                            e ^. entityCollisionObject . unCollisionObject

withEntityTransform :: MonadIO m => Entity s -> (P.Transform -> IO b) -> m b
withEntityTransform e f = liftIO $ bracket (allocateEntityTransform e) P.del f

getEntityOrientation :: MonadIO m => Entity s -> m (L.Quaternion Float)
getEntityOrientation e = do
  (CFloat i, CFloat j, CFloat k, CFloat r) <- withEntityTransform e P.getRotation
  return $ L.Quaternion r (L.V3 i j k)

setEntityOrientation :: MonadIO m => Entity s -> L.Quaternion Float -> m ()
setEntityOrientation e (L.Quaternion r (L.V3 i j k)) = withEntityTransform e $ \t -> do
  P.setRotation t (CFloat i) (CFloat j) (CFloat k) (CFloat r)
  P.coSetWorldTransform (e ^. entityCollisionObject . unCollisionObject) t

getEntityPosition :: MonadIO m => Entity s -> m (L.V3 Float)
getEntityPosition e = withEntityTransform e $ \t -> do
  (CFloat x, CFloat y, CFloat z) <- P.getOrigin t
  return $ L.V3 x y z

setEntityPosition :: MonadIO m => Entity s -> L.V3 Float -> m ()
setEntityPosition e (L.V3 x y z) = withEntityTransform e $ \t -> do
  P.setOrigin t (CFloat x) (CFloat y) (CFloat z)
  liftIO $ P.coSetWorldTransform (e ^. entityCollisionObject . unCollisionObject) t

getEntityLinearVelocity :: MonadIO m => Entity s -> m (L.V3 Float)
getEntityLinearVelocity e = liftIO $
  case e ^. entityRigidBody of
    Nothing -> do
      (CFloat x, CFloat y, CFloat z) <- P.getInterpolationLinearVelocity $
                                        e ^. entityCollisionObject . unCollisionObject
      return $ L.V3 x y z
    Just (RigidBody rb) -> do
      (CFloat x, CFloat y, CFloat z) <- P.rbGetLinearVelocity rb
      return $ L.V3 x y z

setEntityLinearVelocity :: MonadIO m => Entity s -> L.V3 Float -> m ()
setEntityLinearVelocity e (L.V3 x y z) = liftIO $
  case e ^. entityRigidBody of
    Nothing -> P.setInterpolationLinearVelocity
               (e ^. entityCollisionObject . unCollisionObject)
               (CFloat x) (CFloat y) (CFloat z)
    Just (RigidBody rb) -> P.rbSetLinearVelocity rb (CFloat x) (CFloat y) (CFloat z)

getEntityAngularVelocity :: MonadIO m => Entity s -> m (L.V3 Float)
getEntityAngularVelocity e = liftIO $
  case e ^. entityRigidBody of
    Nothing -> do
      (CFloat x, CFloat y, CFloat z) <- P.getInterpolationAngularVelocity $
                                        e ^. entityCollisionObject . unCollisionObject
      return $ L.V3 x y z
    Just (RigidBody rb) -> do
      (CFloat x, CFloat y, CFloat z) <- P.rbGetAngularVelocity rb
      return $ L.V3 x y z

setEntityAngularVelocity :: MonadIO m => Entity s -> L.V3 Float -> m ()
setEntityAngularVelocity e (L.V3 x y z) = liftIO $
  case e ^. entityRigidBody of
    Nothing -> P.setInterpolationAngularVelocity
               (e ^. entityCollisionObject . unCollisionObject)
               (CFloat x) (CFloat y) (CFloat z)
    Just (RigidBody rb) -> P.rbSetAngularVelocity rb (CFloat x) (CFloat y) (CFloat z)

getEntityForce :: MonadIO m => Entity s -> m (L.V3 Float)
getEntityForce e = liftIO $
  case e ^. entityRigidBody of
    Nothing -> return $ L.V3 0 0 0
    Just (RigidBody rb) -> do
      (CFloat x, CFloat y, CFloat z) <- P.getTotalForce rb
      return $ L.V3 x y z

setEntityForce :: MonadIO m => Entity s -> L.V3 Float -> m ()
setEntityForce e (L.V3 x y z) = liftIO $
  case e ^. entityRigidBody of
    Nothing -> return ()
    Just (RigidBody rb) -> do
      P.clearForces rb
      P.applyForce rb (CFloat x) (CFloat y) (CFloat z) 0 0 0
