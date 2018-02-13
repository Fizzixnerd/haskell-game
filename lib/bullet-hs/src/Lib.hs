{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Physics.Bullet
import Debug.Trace
import Control.Monad

someFunc :: IO ()
someFunc = do
  bp :: BroadphaseInterface <- new ()
  traceM "made BroadphaseInterface"
  cc :: CollisionConfiguration <- new ()
  traceM "made CollisionConfiguration"
  d  :: CollisionDispatcher  <- new cc
  traceM "made CollisionDispatcher"
  s  :: ConstraintSolver <- new ()
  traceM "made ConstraintSolver"
  w  :: DynamicsWorld <- new (d, bp, s, cc)
  traceM "made DynamicsWorld"

  sps :: StaticPlaneShape <- new ((0, 1, 0), 1)
  traceM "made StaticPlaneShape"
  spsxform :: Transform <- new ((0, 0, 0, 1), (0, negate 1, 0))
  gms :: MotionState <- new spsxform
  traceM "made StaticPlaneShape MotionState"
  grb <- makeRigidBody sps 0 gms (0, 0, 0)
  traceM "made StaticPlaneShape RigidBody"
  addRigidBody w grb
  traceM "added StaticPlaneShape RigidBody"

  ss  :: SphereShape <- new 1
  ssxform :: Transform <- new ((0, 0, 0, 1), (0, 50, 0))
  sms :: MotionState <- new ssxform
  let mass = 1
  inertia <- calculateLocalInertia ss mass
  srb <- makeRigidBody ss mass sms inertia
  addRigidBody w srb
  traceM "added SphereShape RigidBody"

  pcgo :: PairCachingGhostObject <- new ()
  ssConvex <- sphereShapeToConvexShape ss
  let stepHeight = 1.0

  kcc :: KinematicCharacterController <- new (pcgo, ssConvex, stepHeight)
  setUp kcc 0 1 0
  


  traceM "starting sim"
  forM [1..300] (const $ do
                    stepSimulation w (1 / 60) 10 (1 / 60)
                    trans <- rbGetMotionState srb >>= msAllocateWorldTransform
                    getOrigin trans -- >>= print
                )
  traceM "sim ended"


  freeRigidBody srb
  del sms
  del ss
  freeRigidBody grb
  del gms
  del sps
  del w
  del s
  del cc
  del d
  del bp

