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

newPlayer :: MonadIO m => m (Player s)
newPlayer = liftIO $ do
  pcgo :: P.PairCachingGhostObject <- P.new ()
  startXform <- P.new ((0, 0, 1, 0), (0, 0, 0))
  P.setIdentity startXform
  P.setOrigin startXform 0 10 4
  P.coSetWorldTransform pcgo startXform
  P.del startXform

  playerShape <- P.newCapsuleShape 1 1
  let stepHeight = 0.35
  controller <- P.newKinematicCharacterController pcgo playerShape stepHeight
  P.setLinearDamping controller 0.5
  go <- P.getGhostObject controller
  P.setCollisionShape go playerShape
  P.setUp controller 0 1 0
  let e = Entity
          { _entityChildren = empty
          , _entityGraphics = Nothing
          , _entitySounds   = Nothing
          , _entityLogic    = Nothing
          , _entityRigidBody = Nothing
          , _entityCollisionObject = CollisionObject (P.toCollisionObject go)
          }
  return Player 
    { _playerController = controller
    , _playerEntity = e
    }

destroyPlayer :: MonadIO m => Player s -> m ()
destroyPlayer Player {..} = liftIO $ do
  P.del =<< P.getGhostObject _playerController
  P.del _playerController

allocatePlayerTransform :: MonadIO m => Player s -> m P.Transform
allocatePlayerTransform p = liftIO $
                            P.getGhostObject (p ^. playerController) >>=
                            P.coAllocateWorldTransform

getPlayerOrientation :: MonadIO m => Player s -> m (L.Quaternion CFloat)
getPlayerOrientation p = liftIO $ do
  (i, j, k, r) <- withPlayerTransform p (\t -> P.getRotation t)
  return $ L.Quaternion r (L.V3 i j k)

setPlayerOrientation :: MonadIO m => Player s -> L.Quaternion CFloat -> m ()
setPlayerOrientation p (L.Quaternion r (L.V3 i j k)) = liftIO $
  withPlayerTransform p (\t -> do
                            P.setRotation t i j k r
                            pcgo <- P.getGhostObject $ p ^. playerController
                            P.coSetWorldTransform pcgo t)

withPlayerTransform :: MonadIO m => Player s -> (P.Transform -> IO b) -> m b
withPlayerTransform p f = liftIO $ bracket (allocatePlayerTransform p) P.del f

getPlayerPosition :: MonadIO m => Player s -> m (L.V3 CFloat)
getPlayerPosition p = withPlayerTransform p (\t -> do
                                                (x, y, z) <- P.getOrigin t
                                                return $ L.V3 x y z)

getPlayerOpenGLMatrix :: MonadIO m => Player s -> m (L.M44 CFloat)
getPlayerOpenGLMatrix p = withPlayerTransform p P.getOpenGLMatrix

getPlayerLinearVelocity :: MonadIO m => Player s -> m (L.V3 CFloat)
getPlayerLinearVelocity p = liftIO $ do
  (x, y, z) <- P.kccGetLinearVelocity $ p ^. playerController
  return $ L.V3 x y z

setPlayerLinearVelocity :: MonadIO m => Player s -> L.V3 CFloat -> m ()
setPlayerLinearVelocity p v = liftIO $ P.kccSetLinearVelocity
                              (p ^. playerController)
                              (v ^. L._x)
                              (v ^. L._y)
                              (v ^. L._z)

getPlayerAngularVelocity :: MonadIO m => Player s -> m (L.V3 CFloat)
getPlayerAngularVelocity p = liftIO $ do
  (alpha, beta, gamma) <- P.kccGetAngularVelocity $ p ^. playerController
  return $ L.V3 alpha beta gamma

setPlayerAngularVelocity :: MonadIO m => Player s -> L.V3 CFloat -> m ()
setPlayerAngularVelocity p omega = liftIO $ P.kccSetAngularVelocity
                                   (p ^. playerController)
                                   (omega ^. L._x)
                                   (omega ^. L._y)
                                   (omega ^. L._z)
