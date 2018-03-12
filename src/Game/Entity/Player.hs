{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Entity.Player where

import ClassyPrelude
import Control.Lens
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
  playerShape <- P.newCapsuleShape 1 0.35
  rbci <- P.newRigidBodyConstructionInfo 1 playerMotionState playerShape 0 0 0
  playerRigidBody <- P.newRigidBody rbci
  P.setActivationState playerRigidBody P.activationStateDisableDeactivation
  P.del startXform
  P.del rbci

  let e = Entity
          { _entityChildren = empty
          , _entityGraphics = Nothing
          , _entitySounds   = Nothing
          , _entityLogic    = Nothing
          , _entityRigidBody = Just $ RigidBody playerRigidBody
          , _entityCollisionObject = CollisionObject $ P.toCollisionObject playerRigidBody
          }
  return Player
    { _playerController = playerRigidBody
    , _playerEntity = e
    }

destroyPlayer :: MonadIO m => Player s -> m ()
destroyPlayer Player {..} = liftIO $ P.freeRigidBody _playerController

allocatePlayerTransform :: MonadIO m => Player s -> m P.Transform
allocatePlayerTransform p = liftIO $ P.coAllocateWorldTransform $ p ^. playerController

getPlayerOrientation :: MonadIO m => Player s -> m (L.Quaternion Float)
getPlayerOrientation Player {..} = getEntityOrientation _playerEntity

setPlayerOrientation :: MonadIO m => Player s -> L.Quaternion Float -> m ()
setPlayerOrientation Player {..} = setEntityOrientation _playerEntity

withPlayerTransform :: MonadIO m => Player s -> (P.Transform -> IO b) -> m b
withPlayerTransform p f = liftIO $ bracket (allocatePlayerTransform p) P.del f

getPlayerPosition :: MonadIO m => Player s -> m (L.V3 Float)
getPlayerPosition Player {..} = getEntityPosition _playerEntity

-- getPlayerOpenGLMatrix :: MonadIO m => Player s -> m (L.M44 CFloat)
-- getPlayerOpenGLMatrix p = withPlayerTransform p P.getOpenGLMatrix

getPlayerLinearVelocity :: MonadIO m => Player s -> m (L.V3 Float)
getPlayerLinearVelocity Player {..} = getEntityLinearVelocity _playerEntity

-- | This function doesn't do anything at all even a little bit
setPlayerLinearVelocity :: MonadIO m => Player s -> L.V3 Float -> m ()
setPlayerLinearVelocity Player {..} = setEntityLinearVelocity _playerEntity

playerApplyForce :: MonadIO m => Player s -> L.V3 Float -> m ()
playerApplyForce Player {..} = entityApplyForce _playerEntity

playerJump :: MonadIO m => Player s -> m ()
playerJump p = playerApplyForce p (100 * L.V3 0 1 0)

getPlayerAngularVelocity :: MonadIO m => Player s -> m (L.V3 Float)
getPlayerAngularVelocity Player {..} = getEntityAngularVelocity _playerEntity

setPlayerAngularVelocity :: MonadIO m => Player s -> L.V3 Float -> m ()
setPlayerAngularVelocity Player {..} = setEntityAngularVelocity _playerEntity
