{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Physics.Bullet where

import Control.Exception
import Data.Functor (void)
import Unsafe.Coerce
import Foreign.C.Types
import Foreign hiding (new, void)
import qualified Linear as L

#include "cbullet.h"

{#context lib = "libcbullet"#}

{#pointer *action_interface as ^ newtype#}
{#pointer *box_shape as ^ newtype#}
{#pointer *broadphase_interface as ^ newtype#}
{#pointer *capsule_shape as ^ newtype#}
{#pointer *collision_configuration as ^ newtype#}
{#pointer *collision_dispatcher as ^ newtype#}
{#pointer *collision_object as ^ newtype#}
{#pointer *collision_shape as ^ newtype#}
{#pointer *constraint_solver as ^ newtype#}
{#pointer *convex_shape as ^ newtype#}
{#pointer *dynamics_world as ^ newtype#}
{#pointer *kinematic_character_controller as ^ newtype#}
{#pointer *motion_state as ^ newtype#}
{#pointer *pair_caching_ghost_object as ^ newtype#}
{#pointer *rigid_body as ^ newtype#}
{#pointer *rigid_body_construction_info as ^ newtype#}
{#pointer *serializer as ^ newtype#}
{#pointer *sphere_shape as ^ newtype#}
{#pointer *static_plane_shape as ^ newtype#}
{#pointer *transform as ^ newtype#}
{#pointer *typed_constraint as ^ newtype#}
{#pointer *overlapping_pair_cache as ^ newtype#}
{#pointer *ghost_pair_callback as ^ newtype#}

type Mass = CFloat

class New a x | a -> x where
  new :: x -> IO a
  del :: a -> IO ()

withNew :: New a x => x -> (a -> IO b) -> IO b
withNew x f = bracket (new x) del f

-- | btDbvtBroadphase
{#fun new_broadphase_interface as ^
  {} -> `BroadphaseInterface'
#}

{#fun free_broadphase_interface as ^
 { `BroadphaseInterface' } -> `()'
#}

instance New BroadphaseInterface () where
  new _ = newBroadphaseInterface
  del x = freeBroadphaseInterface x

{#fun get_overlapping_pair_cache as ^
 { `BroadphaseInterface' } -> `OverlappingPairCache'
#}

-- | btOverlappingPairCache
{#fun set_internal_ghost_pair_callback as ^
 { `OverlappingPairCache',
   `GhostPairCallback' } -> `()'
#}

-- | btGhostPairCallback
{#fun new_ghost_pair_callback as ^
 {} -> `GhostPairCallback'
#}

{#fun free_ghost_pair_callback as ^
 { `GhostPairCallback' } -> `()'
#}

instance New GhostPairCallback () where
  new _ = newGhostPairCallback
  del x = freeGhostPairCallback x

-- | btDefaultCollisionConfiguration
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


-- | btDiscreteDynamicsWorld
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

{#fun dw_set_gravity as dwSetGravity
 { `DynamicsWorld',
   `CFloat',
   `CFloat',
   `CFloat' } -> `()'
#}

{#fun step_simulation as stepSimulation
 { `DynamicsWorld',
   `CFloat',
   `Int',
   `CFloat' } -> `Int'
#}

{#fun add_rigid_body as ^
 { `DynamicsWorld',
   `RigidBody' } -> `()'
#}

{#fun get_num_collision_objects as ^
 { `DynamicsWorld' } -> `Int'
#}

{#fun get_collision_object as ^
 { `DynamicsWorld',
   `Int' } -> `CollisionObject'
#}

{#fun add_collision_object as ^
 { `DynamicsWorld',
   `CollisionObject' } -> `()'
#}

{#fun remove_collision_object as ^
 { `DynamicsWorld',
   `CollisionObject' } -> `()'
#}

{#fun add_action as ^
 { `DynamicsWorld',
   `ActionInterface' } -> `()'
#}

{#fun remove_action as ^
 { `DynamicsWorld',
   `ActionInterface' } -> `()'
#}

{#fun get_num_constraints as ^
 { `DynamicsWorld' } -> `Int'
#}

{#fun get_constraint as ^
 { `DynamicsWorld',
   `Int' } -> `TypedConstraint'
#}

{#fun add_constraint as ^
 { `DynamicsWorld',
   `TypedConstraint' } -> `()'
#}

{#fun remove_constraint as ^
 { `DynamicsWorld',
   `TypedConstraint' } -> `()'
#}

{#fun dw_serialize as ^
 { `DynamicsWorld',
   `Serializer' } -> `()'
#}

-- | btCollisionObject
{#fun collision_object_to_rigid_body as ^
 { `CollisionObject' } -> `RigidBody'
#}

{#fun co_allocate_world_transform as ^
 { `CollisionObject' } -> `Transform'
#}

-- | btDefaultMotionState
{#fun new_default_motion_state as ^
 { `Transform' } -> `MotionState'
#}

{#fun free_motion_state as ^
 { `MotionState' } -> `()'
#}

instance New MotionState Transform where
  new t = newDefaultMotionState t
  del x = freeMotionState x

{#fun ms_allocate_world_transform as ^
 { `MotionState' } -> `Transform'
#}

-- | btRigidBodyConstructionInfo
{#fun new_rigid_body_construction_info as ^
 { `CFloat',
   `MotionState',
   `CollisionShape',
   `CFloat',
   `CFloat',
   `CFloat' } -> `RigidBodyConstructionInfo'
#}

{#fun free_rigid_body_construction_info as ^
 { `RigidBodyConstructionInfo' } -> `()'
#}

instance New RigidBodyConstructionInfo (Mass, MotionState, CollisionShape, (CFloat, CFloat, CFloat)) where
  new (m, ms, cs, (x, y, z)) = newRigidBodyConstructionInfo m ms cs x y z
  del x = freeRigidBodyConstructionInfo x

-- | btRigidBody
{#fun new_rigid_body as ^
 { `RigidBodyConstructionInfo' } -> `RigidBody'
#}

{#fun free_rigid_body as ^
 { `RigidBody' } -> `()'
#}

{#fun rb_get_motion_state as ^
 { `RigidBody' } -> `MotionState'
#}

{#fun is_static_object as ^
 { `RigidBody' } -> `Bool'
#}

{#fun is_kinematic_object as ^
 { `RigidBody' } -> `Bool'
#}

{#fun allocate_center_of_mass_transform as ^
 { `RigidBody' } -> `Transform'
#}

-- | btStaticPlaneShape

{#fun new_static_plane_shape as ^
 { `CFloat',
   `CFloat',
   `CFloat',
   `CFloat' } -> `StaticPlaneShape'
#}

{#fun free_static_plane_shape as ^
 { `StaticPlaneShape' } -> `()'
#}

instance New StaticPlaneShape ((CFloat, CFloat, CFloat), CFloat) where
  new ((x, y, z), pc) = newStaticPlaneShape x y z pc
  del x = freeStaticPlaneShape x

-- | btCapsuleShape

{#fun new_capsule_shape as ^
 { `CFloat',
   `CFloat' } -> `CapsuleShape'
#}

{#fun free_capsule_shape as ^
 { `CapsuleShape' } -> `()'
#}

instance New CapsuleShape (CFloat, CFloat) where
  new (r, h) = newCapsuleShape r h
  del x = freeCapsuleShape x

{#fun capsule_shape_to_convex_shape as ^
 { `CapsuleShape' } -> `ConvexShape'
#}

-- | btSphereShape

{#fun new_sphere_shape as ^
 { `CFloat' } -> `SphereShape'
#}

{#fun free_sphere_shape as ^
 { `SphereShape' } -> `()'
#}

instance New SphereShape CFloat where
  new r = newSphereShape r
  del x = freeSphereShape x

{#fun sphere_shape_to_convex_shape as ^
 { `SphereShape' } -> `ConvexShape'
#}

-- | btBoxShape

{#fun new_box_shape as ^
 { `CFloat',
   `CFloat',
   `CFloat' } -> `BoxShape'
#}

{#fun free_box_shape as ^
 { `BoxShape' } -> `()'
#}

instance New BoxShape (CFloat, CFloat, CFloat) where
  new (x, y, z) = newBoxShape x y z
  del x = freeBoxShape x

{#fun box_shape_to_convex_shape as ^
 { `BoxShape' } -> `ConvexShape'
#}

-- | btCollisionShape

{#fun calculate_local_inertia as calculateLocalInertia_
 { `CollisionShape',
   `CFloat',
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek* } -> `()'
#}

calculateLocalInertia :: IsCollisionShape cs => cs -> Mass -> IO (CFloat, CFloat, CFloat)
calculateLocalInertia cs m = calculateLocalInertia_ (toCollisionShape cs) m

class IsCollisionShape cs where
  toCollisionShape :: cs -> CollisionShape
  toCollisionShape cs = unsafeCoerce cs

instance IsCollisionShape SphereShape
instance IsCollisionShape StaticPlaneShape
instance IsCollisionShape BoxShape

-- | btTransform

{#fun new_transform as ^
 { `CFloat', -- quaternion
   `CFloat',
   `CFloat',
   `CFloat',
   `CFloat', -- vector
   `CFloat',
   `CFloat' } -> `Transform'
#}

{#fun free_transform as ^
 { `Transform' } -> `()'
#}

instance New Transform ((CFloat, CFloat, CFloat, CFloat), (CFloat, CFloat, CFloat)) where
  new ((i, j, k, r), (x, y, z)) = newTransform i j k r x y z
  del x = freeTransform x

{#fun get_origin as ^
 { `Transform',
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek* } -> `()'
#}

{#fun set_origin as ^
 { `Transform',
   `CFloat',
   `CFloat',
   `CFloat' } -> `()'
#}

{#fun get_rotation as ^
 { `Transform',
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek* } -> `()'
#}

{#fun set_rotation as ^
 { `Transform',
   `CFloat',
   `CFloat',
   `CFloat',
   `CFloat' } -> `()'
#}

{#fun set_identity as ^
 { `Transform' } -> `()'
#}

allocaMatrix :: (Ptr CFloat -> IO b) -> IO b
allocaMatrix = allocaBytes $ sizeOf (undefined :: L.M44 CFloat)
  
peekMatrix :: Ptr CFloat -> IO (L.M44 CFloat)
peekMatrix p = peek $ castPtr p

{#fun get_opengl_matrix as getOpenGLMatrix
 { `Transform',
   allocaMatrix- `L.M44 CFloat' peekMatrix*} -> `()'
#}

-- | btTypedConstraint

{#fun free_typed_constraint
 { `TypedConstraint' } -> `()'
#}

class IsTypedConstraint tc where
  toTypedConstraint :: tc -> TypedConstraint
  toTypedConstraint = unsafeCoerce

-- | btPoint2PointConstraint

{#fun new_point2point_constraint as newPoint2PointConstraint
 { `RigidBody',
   `CFloat',
   `CFloat',
   `CFloat' } -> `TypedConstraint'
#}

-- | btDefaultSerializer

{#fun new_default_serializer as ^
 {} -> `Serializer'
#}

{#fun free_serializer as ^
 { `Serializer' } -> `()'
#}

instance New Serializer () where
  new _ = newDefaultSerializer
  del x = freeSerializer x

-- more stuff can go for Serializers, but I'll leave it for now...

-- | btPairCachingGhostObject

{#fun new_pair_caching_ghost_object as ^
 {} -> `PairCachingGhostObject'
#}

{#fun free_pair_caching_ghost_object as ^
 { `PairCachingGhostObject' } -> `()'
#}

instance New PairCachingGhostObject () where
  new _ = newPairCachingGhostObject
  del x = freePairCachingGhostObject x

{#fun pair_caching_ghost_object_to_collision_object as ^
 { `PairCachingGhostObject' } -> `CollisionObject'
#}

{#fun pcgo_set_world_transform as ^
 { `PairCachingGhostObject',
   `Transform' } -> `()'
#}

{#fun pcgo_set_collision_shape as ^
 { `PairCachingGhostObject',
   `CollisionShape' } -> `()'
#}

instance IsCollisionObject PairCachingGhostObject

-- | btKinematicCharacterController

{#fun new_kinematic_character_controller as ^
 { `PairCachingGhostObject',
   `ConvexShape',
   `CFloat' } -> `KinematicCharacterController'
#}

{#fun free_kinematic_character_controller as ^
 { `KinematicCharacterController' } -> `()'
#}

instance New KinematicCharacterController (PairCachingGhostObject, ConvexShape, CFloat) where
  new (pcgo, cs, sh) = newKinematicCharacterController pcgo cs sh
  del x = freeKinematicCharacterController x

{#fun set_up as ^
 { `KinematicCharacterController',
   `CFloat',
   `CFloat',
   `CFloat' } -> `()'
#}

{#fun get_up as ^
 { `KinematicCharacterController',
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek* } -> `()'
#}

{#fun set_angular_velocity as ^
 { `KinematicCharacterController',
   `CFloat',
   `CFloat',
   `CFloat' } -> `()'
#}

{#fun get_angular_velocity as ^
 { `KinematicCharacterController',
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek* } -> `()'
#}

{#fun set_linear_velocity as ^
 { `KinematicCharacterController',
   `CFloat',
   `CFloat',
   `CFloat' } -> `()'
#}

{#fun get_linear_velocity as ^
 { `KinematicCharacterController',
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek* } -> `()'
#}

{#fun set_linear_damping as ^
 { `KinematicCharacterController',
   `CFloat' } -> `()'
#}

{#fun get_linear_damping as ^
 { `KinematicCharacterController' } -> `CFloat'
#}

{#fun set_angular_damping as ^
 { `KinematicCharacterController',
   `CFloat' } -> `()'
#}

{#fun get_angular_damping as ^
 { `KinematicCharacterController' } -> `CFloat'
#}

{#fun warp as ^
 { `KinematicCharacterController',
   `CFloat',
   `CFloat',
   `CFloat' } -> `()'
#}

{#fun get_step_height as ^
 { `KinematicCharacterController' } -> `CFloat'
#}

{#fun set_step_height as ^
 { `KinematicCharacterController',
   `CFloat' } -> `()'
#}

{#fun get_fall_speed as ^
 { `KinematicCharacterController' } -> `CFloat'
#}

{#fun set_fall_speed as ^
 { `KinematicCharacterController',
   `CFloat' } -> `()'
#}

{#fun get_jump_speed as ^
 { `KinematicCharacterController' } -> `CFloat'
#}

{#fun set_jump_speed as ^
 { `KinematicCharacterController',
   `CFloat' } -> `()'
#}

{#fun set_max_jump_height as ^
 { `KinematicCharacterController',
   `CFloat' } -> `()'
#}

{#fun can_jump as ^
 { `KinematicCharacterController' } -> `Bool'
#}

{#fun jump as ^
 { `KinematicCharacterController' } -> `()'
#}

{#fun apply_impulse as ^
 { `KinematicCharacterController',
   `CFloat',
   `CFloat',
   `CFloat' } -> `()'
#}

{#fun kcc_set_gravity as ^
 { `KinematicCharacterController',
   `CFloat',
   `CFloat',
   `CFloat' } -> `()'
#}

{#fun get_gravity as ^
 { `KinematicCharacterController',
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek* } -> `()'
#}

{#fun get_max_slope as ^
 { `KinematicCharacterController' } -> `CFloat'
#}

{#fun set_max_slope as ^
 { `KinematicCharacterController',
   `CFloat' } -> `()'
#}

{#fun get_max_penetration_depth as ^
 { `KinematicCharacterController' } -> `CFloat'
#}

{#fun set_max_penetration_depth as ^
 { `KinematicCharacterController',
   `CFloat' } -> `()'
#}

{#fun get_ghost_object as ^
 { `KinematicCharacterController' } -> `PairCachingGhostObject'
#}

{#fun set_use_ghost_sweep_test as ^
 { `KinematicCharacterController',
   `Bool' } -> `()'
#}

{#fun on_ground as ^
 { `KinematicCharacterController' } -> `Bool'
#}

{#fun set_up_interpolate as ^
 { `KinematicCharacterController',
   `Bool' } -> `()'
#}

{#fun kinematic_character_controller_to_action_interface as ^
 { `KinematicCharacterController' } -> `ActionInterface'
#}
