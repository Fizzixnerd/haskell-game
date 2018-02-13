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
  gms :: MotionState <- new ((0, 0, 0, 1), (0, negate 1, 0))
  traceM "made StaticPlaneShape MotionState"
  grb <- newRigidBody sps 0 gms (0, 0, 0)
  traceM "made StaticPlaneShape RigidBody"
  addRigidBody w grb
  traceM "added StaticPlaneShape RigidBody"

  ss  :: SphereShape <- new 1
  sms :: MotionState <- new ((0, 0, 0, 1), (0, 50, 0))
  let mass = 1
  inertia <- calculateLocalInertia ss mass
  srb <- newRigidBody ss mass sms inertia
  addRigidBody w srb
  traceM "added SphereShape RigidBody"

  traceM "starting sim"
  forM [1..300] (const $ do
                    stepSimulation w (1 / 60) 10 (1 / 60)
                    trans <- getMotionState srb >>= getWorldTransform
                    getOrigin trans >>= print)
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

