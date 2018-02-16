{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Entity.Player where

import ClassyPrelude
import Control.Lens
import Foreign.C.Types
import Game.Types
import qualified Linear         as L
import qualified Physics.Bullet as P

newPlayer :: MonadIO m => m Player
newPlayer = liftIO $ do
  pcgo :: P.PairCachingGhostObject <- P.new ()
  startXform <- P.new ((0, 0, 0, 0), (0, 0, 0))
  P.setIdentity startXform
  P.setOrigin startXform 0 0 0
  P.coSetWorldTransform pcgo startXform
  P.del startXform

  playerShape <- P.newCapsuleShape 1 1
  let stepHeight = 0.35
  controller <- P.newKinematicCharacterController pcgo playerShape stepHeight
  go <- P.getGhostObject controller
  P.setCollisionShape go playerShape
  P.setUp controller 0 1 0
  return $ Player controller

destroyPlayer :: MonadIO m => Player -> m ()
destroyPlayer Player {..} = liftIO $ do
  P.del =<< P.getGhostObject _playerPhysicsController
  P.del _playerPhysicsController

allocatePlayerTransform :: MonadIO m => Player -> m P.Transform
allocatePlayerTransform p = liftIO $
                            P.getGhostObject (p ^. playerPhysicsController) >>=
                            P.coAllocateWorldTransform

getPlayerOrientation :: MonadIO m => Player -> m (L.Quaternion CFloat)
getPlayerOrientation p = liftIO $ do
  (i, j, k, r) <- withPlayerTransform p (\t -> P.getRotation t)
  return $ L.Quaternion r (L.V3 i j k)

setPlayerOrientation :: MonadIO m => Player -> L.Quaternion CFloat -> m ()
setPlayerOrientation p (L.Quaternion r (L.V3 i j k)) = liftIO $ 
  withPlayerTransform p (\t -> do
                            P.setRotation t i j k r
                            pcgo <- P.getGhostObject $ p ^. playerPhysicsController
                            P.coSetWorldTransform pcgo t)

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
