{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.World.Physics where

import ClassyPrelude
import Game.Types
-- import Game.Entity.Player
import qualified Physics.Bullet as P
import Control.Lens hiding (cons)

stepPhysicsWorld :: MonadIO m => PhysicsWorld -> m Int
stepPhysicsWorld w = liftIO $ do
  P.stepSimulation (w ^. physicsWorldDynamicsWorld) (1/60) 1 (1/60)

newPhysicsWorld :: MonadIO m => m PhysicsWorld
newPhysicsWorld = liftIO $ do
  _physicsWorldBroadphaseInterface :: P.BroadphaseInterface <- P.new ()
  _physicsWorldGhostPairCallback :: P.GhostPairCallback <- P.new ()
  P.getOverlappingPairCache _physicsWorldBroadphaseInterface >>=
    (flip P.setInternalGhostPairCallback _physicsWorldGhostPairCallback)
  _physicsWorldCollisionConfiguration :: P.CollisionConfiguration <- P.new ()
  _physicsWorldCollisionDispatcher :: P.CollisionDispatcher <- P.new $
    _physicsWorldCollisionConfiguration
  _physicsWorldConstraintSolver :: P.ConstraintSolver <- P.new ()
  _physicsWorldDynamicsWorld :: P.DynamicsWorld <- P.new $
    ( _physicsWorldCollisionDispatcher
    , _physicsWorldBroadphaseInterface
    , _physicsWorldConstraintSolver
    , _physicsWorldCollisionConfiguration )
  let _physicsWorldPlayers = empty
  return $ PhysicsWorld {..}

addPlayerToPhysicsWorld :: MonadIO m => Player -> PhysicsWorld -> m PhysicsWorld
addPlayerToPhysicsWorld p w = liftIO $ do
  P.addCollisionObject (w ^. physicsWorldDynamicsWorld) =<< 
    P.pairCachingGhostObjectToCollisionObject =<<
    P.getGhostObject (p ^. playerPhysicsController)
  P.addAction (w ^. physicsWorldDynamicsWorld) =<<
    P.kinematicCharacterControllerToActionInterface (p ^. playerPhysicsController)
  return $ w & physicsWorldPlayers %~ cons p

destroyPhysicsWorld :: MonadIO m => PhysicsWorld -> m ()
destroyPhysicsWorld PhysicsWorld {..} = liftIO $ do
  -- TODO: This will double free the ghost object!
  -- forM_ _physicsWorldPlayers destroyPlayer 
  P.del _physicsWorldDynamicsWorld
  P.del _physicsWorldCollisionDispatcher
  P.del _physicsWorldCollisionConfiguration
  P.del _physicsWorldBroadphaseInterface
  P.del _physicsWorldGhostPairCallback
