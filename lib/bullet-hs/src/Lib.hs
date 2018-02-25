{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Unsafe.Coerce
import Physics.Bullet
import Debug.Trace
import Control.Monad

someFunc :: IO ()
someFunc = do
  bp :: BroadphaseInterface <- new ()
  traceM "made BroadphaseInterface"
  gpc  :: GhostPairCallback <- new ()
  getOverlappingPairCache bp >>= flip setInternalGhostPairCallback gpc
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
  grb <- newRigidBody =<< newRigidBodyConstructionInfo 0 gms sps 0 0 0
  traceM "made StaticPlaneShape RigidBody"
  addRigidBody w grb
  traceM "added StaticPlaneShape RigidBody"

  ss  :: SphereShape <- new 1
  -- ssxform :: Transform <- new ((0, 0, 0, 1), (0, 50, 0))
  -- sms :: MotionState <- new ssxform
  -- let mass = 1
  -- inertia <- calculateLocalInertia ss mass
  -- srb <- makeRigidBody ss mass sms inertia
  -- addRigidBody w srb
  -- traceM "added SphereShape RigidBody"

  pcgo :: PairCachingGhostObject <- new ()
  startXform <- new ((0, 0, 0, 0), (0, 0, 0))
  setIdentity startXform
  setOrigin startXform 0 50 0
  coSetWorldTransform pcgo startXform
  del startXform
  let stepHeight = 0.35
  kcc :: KinematicCharacterController <- newKinematicCharacterController pcgo ss stepHeight
  traceM "Made KinematicCharacterController."
  pcgo <- getGhostObject kcc
  setCollisionShape pcgo ss
  setUp kcc 0 1 0
  addCollisionObject w pcgo
  addAction w kcc

  traceM "starting sim"
  forM [1..300] (\x -> if x /= 200 then do
                    stepSimulation w (1 / 60) 1 (1 / 60)
                    xform <- getGhostObject kcc >>=
                             coAllocateWorldTransform
                    getOrigin xform >>= print
                    del xform
                       else jump kcc)
  traceM "sim ended"

  --freeRigidBody srb
  --del sms
  del ss
  freeRigidBody grb
  del gms
  del sps
  del w
  del s
  del cc
  del d
  del bp

