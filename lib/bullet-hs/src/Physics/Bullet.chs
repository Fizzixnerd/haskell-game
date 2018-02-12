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
{#pointer *default_collision_configuration as ^ newtype#}
{#pointer *collision_dispatcher as ^ newtype#}
{#pointer *sequential_impulse_constraint_solver as ^ newtype#}
{#pointer *discrete_dynamics_world as ^ newtype#}
{#pointer *default_motion_state as ^ newtype#}
{#pointer *static_plane_shape as ^ newtype#}
{#pointer *sphere_shape as ^ newtype#}
{#pointer *collision_shape as ^ newtype#}
{#pointer *rigid_body as ^ newtype#}

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
 {} -> `DefaultCollisionConfiguration'
#}

{#fun free_default_collision_configuration as ^
 { `DefaultCollisionConfiguration' } -> `()'
#}

instance New DefaultCollisionConfiguration () where
  new _ = newDefaultCollisionConfiguration
  del x = freeDefaultCollisionConfiguration x

{#fun new_collision_dispatcher as ^
 { `DefaultCollisionConfiguration' } -> `CollisionDispatcher'
#}

{#fun free_collision_dispatcher as ^
 { `CollisionDispatcher' } -> `()'
#}

instance New CollisionDispatcher DefaultCollisionConfiguration where
  new x = newCollisionDispatcher x
  del x = freeCollisionDispatcher x

{#fun new_sequential_impulse_constraint_solver as ^
 {} -> `SequentialImpulseConstraintSolver'
#}

{#fun free_sequential_impulse_constraint_solver as ^
 { `SequentialImpulseConstraintSolver' } -> `()'
#}

instance New SequentialImpulseConstraintSolver () where
  new _ = newSequentialImpulseConstraintSolver
  del x = freeSequentialImpulseConstraintSolver x

{#fun new_discrete_dynamics_world as ^
 { `CollisionDispatcher',
   `BroadphaseInterface',
   `SequentialImpulseConstraintSolver',
   `DefaultCollisionConfiguration' } -> `DiscreteDynamicsWorld'
#}

{#fun free_discrete_dynamics_world as ^
 { `DiscreteDynamicsWorld' } -> `()'
#}

instance New DiscreteDynamicsWorld ( CollisionDispatcher
                                   , BroadphaseInterface
                                   , SequentialImpulseConstraintSolver
                                   , DefaultCollisionConfiguration) where
  new (cd, bi, sics, dcc) = newDiscreteDynamicsWorld cd bi sics dcc
  del x = freeDiscreteDynamicsWorld x

{#fun new_default_motion_state as ^
 { `Float', -- Quaternion
   `Float',
   `Float',
   `Float',
   `Float', -- Vector
   `Float',
   `Float' } -> `DefaultMotionState'
#}

{#fun free_default_motion_state as ^
 { `DefaultMotionState' } -> `()'
#}

instance New DefaultMotionState ((Float,Float,Float,Float),(Float,Float,Float)) where
  new ((r, i, j, k), (x, y, z)) = newDefaultMotionState r i j k x y z
  del x = freeDefaultMotionState x

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

class IsCollisionShape cs where
  calculateLocalInertia :: cs -> Mass -> IO (CFloat, CFloat, CFloat)
  calculateLocalInertia ss m = calculateLocalInertia_ (unsafeCoerce ss :: CollisionShape) m

instance IsCollisionShape SphereShape

