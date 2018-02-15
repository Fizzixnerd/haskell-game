{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Entity.Player where

import ClassyPrelude
import Game.Types
import qualified Physics.Bullet as P
import qualified Linear as L
import Foreign.C.Types
import Control.Lens
import Unsafe.Coerce

newPlayer :: MonadIO m => m Player
newPlayer = liftIO $ do
  pcgo :: P.PairCachingGhostObject <- P.new ()
  startXform <- P.new ((0, 0, 0, 0), (0, 0, 0))
  P.setIdentity startXform
  P.setOrigin startXform 0 0 0
  P.pcgoSetWorldTransform pcgo startXform
  P.del startXform

  playerShape <- P.newCapsuleShape 1 1
  psConvex <- P.capsuleShapeToConvexShape playerShape
  let stepHeight = 0.35
  controller :: P.KinematicCharacterController <- P.new (pcgo, psConvex, stepHeight)
  go <- P.getGhostObject controller
  P.pcgoSetCollisionShape go (unsafeCoerce playerShape) -- TODO: Fix this nonsense
  P.setUp controller 0 1 0
  return $ Player controller

destroyPlayer :: MonadIO m => Player -> m ()
destroyPlayer Player {..} = liftIO $ do
  P.del =<< P.getGhostObject _playerPhysicsController
  P.del _playerPhysicsController

allocatePlayerTransform :: MonadIO m => Player -> m P.Transform
allocatePlayerTransform p = liftIO $
                            P.getGhostObject (p ^. playerPhysicsController) >>=
                            P.pairCachingGhostObjectToCollisionObject >>=
                            P.coAllocateWorldTransform

getPlayerOrientation :: MonadIO m => Player -> m (L.Quaternion CFloat)
getPlayerOrientation p = liftIO $ do
  (i, j, k, r) <- withPlayerTransform p (\t -> P.getRotation t)
  return $ L.Quaternion r (L.V3 i j k)

setPlayerOrientation :: MonadIO m => Player -> m (L.Quaternion CFloat)
setPlayerOrientation p (L.Quaternion r (L.V3 i j k)) =
  withPlayerTransform p (\t -> do
                            P.setRotation t i j k r
                            P.pcgoSetWorldTransform (p ^. playerPhysicsController) t)

withPlayerTransform :: MonadIO m => Player -> (P.Transform -> IO b) -> m b
withPlayerTransform p f = liftIO $ bracket (allocatePlayerTransform p) P.del f

getPlayerPosition :: MonadIO m => Player -> m (L.V3 CFloat)
getPlayerPosition p = withPlayerTransform p (\t -> do
                                                (x, y, z) <- P.getOrigin t
                                                return $ L.V3 x y z)

getPlayerOpenGLMatrix :: MonadIO m => Player -> m (L.M44 CFloat)
getPlayerOpenGLMatrix p = withPlayerTransform p P.getOpenGLMatrix

getPlayerLinearVelocity :: MonadIO m => Player -> m (L.V3 CFloat)
getPlayerLinearVelocity p = liftIO $ do
  (x, y, z) <- P.getLinearVelocity $ p ^. playerPhysicsController
  return $ L.V3 x y z

setPlayerLinearVelocity :: MonadIO m => Player -> L.V3 CFloat -> m ()
setPlayerLinearVelocity p v = liftIO $ P.setLinearVelocity
                              (p ^. playerPhysicsController)
                              (v ^. L._x)
                              (v ^. L._y)
                              (v ^. L._z)

getPlayerAngularVelocity :: MonadIO m => Player -> m (L.V3 CFloat)
getPlayerAngularVelocity p = liftIO $ do
  (alpha, beta, gamma) <- P.getAngularVelocity $ p ^. playerPhysicsController
  return $ L.V3 alpha beta gamma

setPlayerAngularVelocity :: MonadIO m => Player -> L.V3 CFloat -> m ()
setPlayerAngularVelocity p omega = liftIO $ P.setAngularVelocity
                                   (p ^. playerPhysicsController)
                                   (omega ^. L._x)
                                   (omega ^. L._y)
                                   (omega ^. L._z)
