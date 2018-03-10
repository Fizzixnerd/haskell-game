{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Entity.Player where

import ClassyPrelude
import Control.Lens
import Foreign.C.Types
import Game.Types
import Game.Entity
import qualified Linear         as L
import qualified Physics.Bullet as P

newPlayer :: MonadIO m => m (Player s)
newPlayer = liftIO $ do
  --  pcgo :: P.PairCachingGhostObject <- P.new ()
  startXform <- P.new ((0, 0, 0, 0), (0, 0, 0))
  P.setIdentity startXform
  P.setOrigin startXform 0 4 4
  playerMotionState <- P.new startXform
  playerShape <- P.newCapsuleShape 2 0.35
  rbci <- P.newRigidBodyConstructionInfo 1 playerMotionState playerShape 0 0 0
  playerRigidBody <- P.newRigidBody rbci
  P.del startXform
  P.del rbci

  -- let stepHeight = 0.35
  -- controller <- P.newKinematicCharacterController pcgo playerShape stepHeight
  let e = Entity
          { _entityChildren = empty
          , _entityGraphics = Nothing
          , _entitySounds   = Nothing
          , _entityLogic    = Just Lfx
                              { _lfxScripts =
                                  fromList [ \model_ -> do
                                               entityLocalClosestRayCast model_ (L.V3 0 (-3) 0) $
                                                 const $ entityApplyForce model_ (L.V3 0 8.75 0)
                                               return model_
                                           ]
                              }
          , _entityRigidBody = Just $ RigidBody playerRigidBody
          , _entityCollisionObject = CollisionObject (P.toCollisionObject playerRigidBody)
          }
  return Player
    { _playerController = playerRigidBody
    , _playerEntity = e
    }

destroyPlayer :: MonadIO m => Player s -> m ()
destroyPlayer Player {..} = liftIO $ P.freeRigidBody _playerController

allocatePlayerTransform :: MonadIO m => Player s -> m P.Transform
allocatePlayerTransform p = liftIO $ P.coAllocateWorldTransform $ p ^. playerController

getPlayerOrientation :: MonadIO m => Player s -> m (L.Quaternion CFloat)
getPlayerOrientation p = liftIO $ do
  (i, j, k, r) <- withPlayerTransform p P.getRotation
  return $ L.Quaternion r (L.V3 i j k)

setPlayerOrientation :: MonadIO m => Player s -> L.Quaternion CFloat -> m ()
setPlayerOrientation p (L.Quaternion r (L.V3 i j k)) = liftIO $
  withPlayerTransform p (\t -> do
                            P.setRotation t i j k r
                            P.coSetWorldTransform (p ^. playerController) t)

withPlayerTransform :: MonadIO m => Player s -> (P.Transform -> IO b) -> m b
withPlayerTransform p f = liftIO $ bracket (allocatePlayerTransform p) P.del f

getPlayerPosition :: MonadIO m => Player s -> m (L.V3 CFloat)
getPlayerPosition p = withPlayerTransform p $ \t -> do
  (x, y, z) <- P.getOrigin t
  return $ L.V3 x y z

-- getPlayerOpenGLMatrix :: MonadIO m => Player s -> m (L.M44 CFloat)
-- getPlayerOpenGLMatrix p = withPlayerTransform p P.getOpenGLMatrix

getPlayerLinearVelocity :: MonadIO m => Player s -> m (L.V3 CFloat)
getPlayerLinearVelocity p = liftIO $ do
  (x, y, z) <- P.rbGetLinearVelocity $ p ^. playerController
  return $ L.V3 x y z

-- | This function doesn't do anything at all even a little bit
setPlayerLinearVelocity :: MonadIO m => Player s -> L.V3 CFloat -> m ()
setPlayerLinearVelocity p v = liftIO $ P.rbSetLinearVelocity
                              (p ^. playerController)
                              (v ^. L._x)
                              (v ^. L._y)
                              (v ^. L._z)

playerApplyForce :: MonadIO m => Player s -> L.V3 CFloat -> m ()
playerApplyForce p f = liftIO $ P.applyForce
                       (p ^. playerController)
                       (f ^. L._x)
                       (f ^. L._y)
                       (f ^. L._z)
                       0
                       0
                       0

playerJump :: MonadIO m => Player s -> m ()
playerJump p = playerApplyForce p (100 * L.V3 0 1 0)

getPlayerAngularVelocity :: MonadIO m => Player s -> m (L.V3 CFloat)
getPlayerAngularVelocity p = liftIO $ do
  (alpha, beta, gamma) <- P.rbGetAngularVelocity $ p ^. playerController
  return $ L.V3 alpha beta gamma

setPlayerAngularVelocity :: MonadIO m => Player s -> L.V3 CFloat -> m ()
setPlayerAngularVelocity p omega = liftIO $ P.rbSetAngularVelocity
                                   (p ^. playerController)
                                   (omega ^. L._x)
                                   (omega ^. L._y)
                                   (omega ^. L._z)
