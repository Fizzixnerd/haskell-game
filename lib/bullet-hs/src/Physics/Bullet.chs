{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Physics.Bullet where

import Unsafe.Coerce
import Foreign.C.Types
import Foreign hiding (new)

#include "cbullet.h"

{#context lib = "libcbullet"#}

{#pointer *broadphase_interface as ^ newtype#}
{#pointer *collision_configuration as ^ newtype#}
{#pointer *collision_dispatcher as ^ newtype#}
{#pointer *constraint_solver as ^ newtype#}
{#pointer *dynamics_world as ^ newtype#}
{#pointer *motion_state as ^ newtype#}
{#pointer *static_plane_shape as ^ newtype#}
{#pointer *sphere_shape as ^ newtype#}
{#pointer *collision_shape as ^ newtype#}
{#pointer *rigid_body as ^ newtype#}
{#pointer *transform as ^ newtype#}

class New a x | a -> x where
  new :: x -> IO a
  del :: a -> IO ()

withNew :: New a x => x -> (a -> IO b) -> IO b
withNew x f = do
  a <- new x
  b <- f a
  del a
  return b

{#fun new_broadphase_interface as ^
  {} -> `BroadphaseInterface'
#}

{#fun free_broadphase_interface as ^
 { `BroadphaseInterface' } -> `()'
#}

instance New BroadphaseInterface () where
  new _ = newBroadphaseInterface
  del x = freeBroadphaseInterface x

{#fun new_default_collision_configuration as ^
 {} -> `CollisionConfiguration'
#}

{#fun free_collision_configuration as ^
 { `CollisionConfiguration' } -> `()'
#}

instance New CollisionConfiguration () where
  new _ = newDefaultCollisionConfiguration
  del x = freeCollisionConfiguration x

{#fun new_collision_dispatcher as ^
 { `CollisionConfiguration' } -> `CollisionDispatcher'
#}

{#fun free_collision_dispatcher as ^
 { `CollisionDispatcher' } -> `()'
#}

instance New CollisionDispatcher CollisionConfiguration where
  new x = newCollisionDispatcher x
  del x = freeCollisionDispatcher x

{#fun new_sequential_impulse_constraint_solver as ^
 {} -> `ConstraintSolver'
#}

{#fun free_constraint_solver as ^
 { `ConstraintSolver' } -> `()'
#}

instance New ConstraintSolver () where
  new _ = newSequentialImpulseConstraintSolver
  del x = freeConstraintSolver x

{#fun new_discrete_dynamics_world as ^
 { `CollisionDispatcher',
   `BroadphaseInterface',
   `ConstraintSolver',
   `CollisionConfiguration' } -> `DynamicsWorld'
#}

{#fun free_dynamics_world as ^
 { `DynamicsWorld' } -> `()'
#}

instance New DynamicsWorld ( CollisionDispatcher
                           , BroadphaseInterface
                           , ConstraintSolver
                           , CollisionConfiguration) where
  new (cd, bi, sics, dcc) = newDiscreteDynamicsWorld cd bi sics dcc
  del x = freeDynamicsWorld x

{#fun set_gravity as setGravity
 { `DynamicsWorld',
   `Float',
   `Float',
   `Float' } -> `()'
#}

{#fun step_simulation as stepSimulation
 { `DynamicsWorld',
   `Float',
   `Int',
   `Float' } -> `Int'
#}

{#fun add_rigid_body as ^
 { `DynamicsWorld',
   `RigidBody' } -> `()'
#}

{#fun new_default_motion_state as ^
 { `Float', -- Quaternion
   `Float',
   `Float',
   `Float',
   `Float', -- Vector
   `Float',
   `Float' } -> `MotionState'
#}

{#fun free_motion_state as ^
 { `MotionState' } -> `()'
#}

instance New MotionState ((Float,Float,Float,Float),(Float,Float,Float)) where
  new ((r, i, j, k), (x, y, z)) = newDefaultMotionState r i j k x y z
  del x = freeMotionState x

{#fun get_world_transform as ^
 { `MotionState' } -> `Transform'
#}

{#fun get_origin as ^
 { `Transform',
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek* } -> `()'
#}

{#fun new_static_plane_shape as ^
 { `Float',
   `Float',
   `Float',
   `Float' } -> `StaticPlaneShape'
#}

{#fun free_static_plane_shape as ^
 { `StaticPlaneShape' } -> `()'
#}

instance New StaticPlaneShape ((Float, Float, Float), Float) where
  new ((x, y, z), pc) = newStaticPlaneShape x y z pc
  del x = freeStaticPlaneShape x

{#fun new_sphere_shape as ^
 { `Float' } -> `SphereShape'
#}

{#fun free_sphere_shape as ^
 { `SphereShape' } -> `()'
#}

instance New SphereShape Float where
  new r = newSphereShape r
  del x = freeSphereShape x

type Mass = Float

{#fun calculate_local_inertia as calculateLocalInertia_
 { `CollisionShape',
   `Float',
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek* } -> `()'
#}

{#fun new_rigid_body as newRigidBody_
 { `Float',
   `MotionState',
   `CollisionShape',
   `CFloat',
   `CFloat',
   `CFloat' } -> `RigidBody'
#}

{#fun free_rigid_body as ^
 { `RigidBody' } -> `()'
#}

{#fun get_motion_state as ^
 { `RigidBody' } -> `MotionState'
#}

class IsCollisionShape cs where
  calculateLocalInertia :: cs -> Mass -> IO (CFloat, CFloat, CFloat)
  calculateLocalInertia ss m = calculateLocalInertia_ (unsafeCoerce ss :: CollisionShape) m

  newRigidBody :: cs
               -> Mass
               -> MotionState
               -> (CFloat, CFloat, CFloat)
               -> IO RigidBody
  newRigidBody cs m dms (x, y, z) = newRigidBody_ m dms (unsafeCoerce cs :: CollisionShape) x y z

instance IsCollisionShape SphereShape
instance IsCollisionShape StaticPlaneShape
