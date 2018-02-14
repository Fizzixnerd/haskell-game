{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.World.Physics where

import ClassyPrelude
import Game.Types
import qualified Physics.Bullet as P
import Control.Lens

stepPhysicsWorld :: MonadIO m => PhysicsWorld -> m Int
stepPhysicsWorld w = liftIO $ do
  P.stepSimulation (w ^. physicsWorldDynamicsWorld) (1/60) 1 (1/60)

newPhysicsWorld :: MonadIO m => m PhysicsWorld
newPhysicsWorld = liftIO $ do
  bp :: P.BroadphaseInterface <- P.new ()
  gpc :: P.GhostPairCallback <- P.new ()
  P.getOverlappingPairCache bp >>= (flip P.setInternalGhostPairCallback gpc)
  cc :: P.CollisionConfiguration <- P.new ()
  d :: P.CollisionDispatcher <- P.new cc
  s :: P.ConstraintSolver <- P.new ()
  w :: P.DynamicsWorld <- P.new (d, bp, s, cc)
  return $ PhysicsWorld w

addPlayerToPhysicsWorld :: MonadIO m => PhysicsWorld -> m Player
addPlayerToPhysicsWorld w = liftIO $ do
  pcgo :: P.PairCachingGhostObject <- P.new ()
  startXform <- P.new ((0, 0, 0, 0), (0, 0, 0))
  P.setIdentity startXform
  P.setOrigin startXform 0 0 0
  P.pcgoSetWorldTransform pcgo startXform
  P.del startXform
  undefined
  
