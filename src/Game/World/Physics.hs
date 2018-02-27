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
import Control.Lens hiding (snoc)

stepPhysicsWorld :: MonadIO m => PhysicsWorld s -> m Int
stepPhysicsWorld w = liftIO $ do
  P.stepSimulation (w ^. physicsWorldDynamicsWorld) (1/60) 1 (1/60)

newPhysicsWorld :: MonadIO m => m (PhysicsWorld s)
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
      _physicsWorldEntities = empty
  return $ PhysicsWorld {..}

destroyPhysicsWorld :: MonadIO m => PhysicsWorld s -> m ()
destroyPhysicsWorld PhysicsWorld {..} = liftIO $ do
  -- TODO: This will double free the ghost object!
  -- forM_ _physicsWorldPlayers destroyPlayer 
  P.del _physicsWorldDynamicsWorld
  P.del _physicsWorldCollisionDispatcher
  P.del _physicsWorldCollisionConfiguration
  P.del _physicsWorldBroadphaseInterface
  P.del _physicsWorldGhostPairCallback

updateIndex :: MonadIO m => Entity s -> PhysicsWorld s -> m (PhysicsWorld s)
updateIndex e pw = do
  let pw' = pw & physicsWorldEntities %~ (flip snoc e)
      n   = (length $ pw' ^. physicsWorldEntities) - 1
  liftIO $ P.setUserIndex (e ^. entityCollisionObject . unCollisionObject) n
  return pw'

addPlayerToPhysicsWorld :: MonadIO m
                        => Player s
                        -> PhysicsWorld s
                        -> m (PhysicsWorld s)
addPlayerToPhysicsWorld p w = liftIO $ do
  go <- P.getGhostObject $ p ^. playerController
  P.addCollisionObject (w ^. physicsWorldDynamicsWorld) go
  P.addAction (w ^. physicsWorldDynamicsWorld) (p ^. playerController)
  let w' = w & physicsWorldPlayers %~ (flip snoc p)
  w'' <- updateIndex (p ^. playerEntity) w'
  return w''

addGiantFeaturelessPlaneToPhysicsWorld :: MonadIO m
                                       => GiantFeaturelessPlane s
                                       -> PhysicsWorld s
                                       -> m (PhysicsWorld s)
addGiantFeaturelessPlaneToPhysicsWorld g@(GiantFeaturelessPlane gfp _) w = liftIO $ do
  P.addRigidBody (w ^. physicsWorldDynamicsWorld) gfp
  w' <- updateIndex (g ^. giantFeaturelessPlaneEntity) w
  return $ w' & physicsWorldGiantFeaturelessPlanes %~ (flip snoc g)

addCameraToPhysicsWorld :: MonadIO m => Camera s -> PhysicsWorld s -> m (PhysicsWorld s)
addCameraToPhysicsWorld cam w = liftIO $ do
  go <- P.getGhostObject (cam ^. cameraController)
  P.addCollisionObject (w ^. physicsWorldDynamicsWorld) go
  P.addAction (w ^. physicsWorldDynamicsWorld) (cam ^. cameraController)
  w' <- updateIndex (cam ^. cameraEntity) w
  return $ w' & physicsWorldCameras %~ (flip snoc cam)

setGravityPhysicsWorld :: MonadIO m => L.V3 CFloat -> PhysicsWorld s -> m ()
setGravityPhysicsWorld (L.V3 x y z) p = liftIO $ P.dwSetGravity (p ^. physicsWorldDynamicsWorld) x y z

addCollisionObjectToPhysicsWorld :: (MonadIO m, P.IsCollisionObject co)
                                 => co
                                 -> PhysicsWorld s 
                                 -> m (PhysicsWorld s)
addCollisionObjectToPhysicsWorld co pw = liftIO $ do
  P.addCollisionObject (pw ^. physicsWorldDynamicsWorld) co
  let pw' = pw & physicsWorldCollisionObjects %~ (flip snoc (P.toCollisionObject co))
  return pw'

addRigidBodyToPhysicsWorld :: MonadIO m
                           => P.RigidBody
                           -> PhysicsWorld s
                           -> m (PhysicsWorld s)
addRigidBodyToPhysicsWorld rb pw = liftIO $ do
  P.addRigidBody (pw ^. physicsWorldDynamicsWorld) rb
  return $ pw & physicsWorldRigidBodies %~ (flip snoc rb)

addEntityToPhysicsWorld ::
  MonadIO m => Entity s -> PhysicsWorld s -> m (PhysicsWorld s)
addEntityToPhysicsWorld e pw = do
  pw' <- case e ^. entityRigidBody of
           Nothing -> do
             addCollisionObjectToPhysicsWorld (e ^. entityCollisionObject . unCollisionObject) pw
           Just rb -> do
             addRigidBodyToPhysicsWorld (rb ^. unRigidBody) pw
  let pw'' = pw' & physicsWorldEntities %~ (flip snoc e)
  return pw''
    
