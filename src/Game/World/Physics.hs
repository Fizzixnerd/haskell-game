{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.World.Physics where

import ClassyPrelude
import Game.Types
import qualified Linear as L
import Foreign.C.Types
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
      _physicsWorldGiantFeaturelessPlanes = empty
      _physicsWorldCameras = empty
      _physicsWorldRigidBodies = empty
      _physicsWorldCollisionObjects = empty
  return $ PhysicsWorld {..}

destroyPhysicsWorld :: MonadIO m => PhysicsWorld -> m ()
destroyPhysicsWorld PhysicsWorld {..} = liftIO $ do
  -- TODO: This will double free the ghost object!
  -- forM_ _physicsWorldPlayers destroyPlayer 
  P.del _physicsWorldDynamicsWorld
  P.del _physicsWorldCollisionDispatcher
  P.del _physicsWorldCollisionConfiguration
  P.del _physicsWorldBroadphaseInterface
  P.del _physicsWorldGhostPairCallback

addPlayerToPhysicsWorld :: MonadIO m => Player -> PhysicsWorld -> m PhysicsWorld
addPlayerToPhysicsWorld p w = liftIO $ do
  P.addCollisionObject (w ^. physicsWorldDynamicsWorld) =<< 
    P.getGhostObject (p ^. playerController)
  P.addAction (w ^. physicsWorldDynamicsWorld) (p ^. playerController)
  return $ w & physicsWorldPlayers %~ cons p

addGiantFeaturelessPlaneToPhysicsWorld :: MonadIO m => GiantFeaturelessPlane -> PhysicsWorld -> m PhysicsWorld
addGiantFeaturelessPlaneToPhysicsWorld g@(GiantFeaturelessPlane gfp) w = liftIO $ do
  P.addRigidBody (w ^. physicsWorldDynamicsWorld) gfp
  return $ w & physicsWorldGiantFeaturelessPlanes %~ cons g

addCameraToPhysicsWorld :: MonadIO m => Camera -> PhysicsWorld -> m PhysicsWorld
addCameraToPhysicsWorld cam w = liftIO $ do
  P.addCollisionObject (w ^. physicsWorldDynamicsWorld) =<<
    P.getGhostObject (cam ^. cameraController)
  P.addAction (w ^. physicsWorldDynamicsWorld) (cam ^. cameraController)
  return $ w & physicsWorldCameras %~ cons cam

setGravityPhysicsWorld :: MonadIO m => L.V3 CFloat -> PhysicsWorld -> m ()
setGravityPhysicsWorld (L.V3 x y z) p = liftIO $ P.dwSetGravity (p ^. physicsWorldDynamicsWorld) x y z

addCollisionObjectToPhysicsWorld :: (MonadIO m, P.IsCollisionObject co)
                                 => co
                                 -> PhysicsWorld
                                 -> m PhysicsWorld
addCollisionObjectToPhysicsWorld co pw = do
  liftIO $ P.addCollisionObject (pw ^. physicsWorldDynamicsWorld) co
  return $ pw & physicsWorldCollisionObjects %~ cons (P.toCollisionObject co)

addRigidBodyToPhysicsWorld :: MonadIO m
                           => P.RigidBody
                           -> PhysicsWorld
                           -> m PhysicsWorld
addRigidBodyToPhysicsWorld rb pw = do
  liftIO $ P.addRigidBody (pw ^. physicsWorldDynamicsWorld) rb
  return $ pw & physicsWorldRigidBodies %~ cons rb
