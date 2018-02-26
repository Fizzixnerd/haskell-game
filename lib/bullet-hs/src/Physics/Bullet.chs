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
{#pointer *point2point_constraint as Point2PointConstraint newtype#}
{#pointer *overlapping_pair_cache as ^ newtype#}
{#pointer *ghost_pair_callback as ^ newtype#}
{#pointer *collision_world as ^ newtype#}
{#pointer *closest_ray_result_callback as ^ newtype#}
{#pointer *ray_result_callback as ^ newtype#}

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

-- | btCollisionDispatcher
{#fun new_collision_dispatcher as ^
 { `CollisionConfiguration' } -> `CollisionDispatcher'
#}

{#fun free_collision_dispatcher as ^
 { `CollisionDispatcher' } -> `()'
#}

instance New CollisionDispatcher CollisionConfiguration where
  new x = newCollisionDispatcher x
  del x = freeCollisionDispatcher x

-- | btSequentialImpulseConstraintSolver
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
instance IsCollisionWorld DynamicsWorld

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

{#fun add_collision_object as addCollisionObject_
 { `DynamicsWorld',
   `CollisionObject' } -> `()'
#}

addCollisionObject :: IsCollisionObject co => DynamicsWorld -> co -> IO ()
addCollisionObject dw = addCollisionObject_ dw . toCollisionObject

{#fun remove_collision_object as removeCollisionObject_
 { `DynamicsWorld',
   `CollisionObject' } -> `()'
#}

removeCollisionObject :: IsCollisionObject co => DynamicsWorld -> co -> IO ()
removeCollisionObject dw = removeCollisionObject dw . toCollisionObject

{#fun add_action as addAction_
 { `DynamicsWorld',
   `ActionInterface' } -> `()'
#}

addAction :: IsActionInterface ai => DynamicsWorld -> ai -> IO ()
addAction dw = addAction_ dw . toActionInterface

{#fun remove_action as removeAction_
 { `DynamicsWorld',
   `ActionInterface' } -> `()'
#}

removeAction :: IsActionInterface ai => DynamicsWorld -> ai -> IO ()
removeAction dw = removeAction_ dw . toActionInterface

{#fun get_num_constraints as ^
 { `DynamicsWorld' } -> `Int'
#}

{#fun get_constraint as ^
 { `DynamicsWorld',
   `Int' } -> `TypedConstraint'
#}

{#fun add_constraint as addConstraint_
 { `DynamicsWorld',
   `TypedConstraint' } -> `()'
#}

addConstraint :: IsTypedConstraint tc => DynamicsWorld -> tc -> IO ()
addConstraint dw = addConstraint_ dw . toTypedConstraint

{#fun remove_constraint as removeConstraint_
 { `DynamicsWorld',
   `TypedConstraint' } -> `()'
#}

removeConstraint :: IsTypedConstraint tc => DynamicsWorld -> tc -> IO ()
removeConstraint dw = removeConstraint_ dw . toTypedConstraint

{#fun dw_serialize as ^
 { `DynamicsWorld',
   `Serializer' } -> `()'
#}

-- | btCollisionWorld::ClosestRayResultCallback
instance IsRayResultCallback ClosestRayResultCallback

{#fun new_closest_ray_result_callback as ^
 { `CFloat',
   `CFloat',
   `CFloat',
   `CFloat',
   `CFloat',
   `CFloat' } -> `ClosestRayResultCallback'
#}

{#fun free_closest_ray_result_callback as ^
 { `ClosestRayResultCallback' } -> `()'
#}

instance New ClosestRayResultCallback ((CFloat, CFloat, CFloat), (CFloat, CFloat, CFloat)) where
  new ((fx, fy, fz), (tx, ty, tz)) = newClosestRayResultCallback fx fy fz tx ty tz
  del x = freeClosestRayResultCallback x

-- | btCollisionWorld::RayResultCallback
class IsRayResultCallback rrc where
  toRayResultCallback :: rrc -> RayResultCallback
  toRayResultCallback = unsafeCoerce

{#fun rrc_has_hit as rrcHasHit_
 { `RayResultCallback' } -> `Bool'
#}

rrcHasHit :: IsRayResultCallback rrc => rrc -> IO Bool
rrcHasHit = rrcHasHit_ . toRayResultCallback

{#fun rrc_get_hit as rrcGetHit_
 { `RayResultCallback' } -> `CollisionObject'
#}

rrcGetHit :: IsRayResultCallback rrc => rrc -> IO CollisionObject
rrcGetHit = rrcGetHit_ . toRayResultCallback

-- | btCollisionWorld
class IsCollisionWorld cw where
  toCollisionWorld :: cw -> CollisionWorld
  toCollisionWorld = unsafeCoerce

{#fun ray_test as rayTest_
 { `CollisionWorld',
   `CFloat',
   `CFloat',
   `CFloat',
   `CFloat',
   `CFloat',
   `CFloat',
   `RayResultCallback' } -> `()'
#}

rayTest :: (IsCollisionWorld cw, IsRayResultCallback rrc)
        => cw
        -> CFloat
        -> CFloat
        -> CFloat
        -> CFloat
        -> CFloat
        -> CFloat
        -> rrc
        -> IO ()
rayTest cw fx fy fz tx ty tz rrc = 
  rayTest_ (toCollisionWorld cw) fx fy fz tx ty tz (toRayResultCallback rrc)

-- | btCollisionObject
class IsCollisionObject cs where
  toCollisionObject :: cs -> CollisionObject
  toCollisionObject = unsafeCoerce

instance IsCollisionObject CollisionObject

{#fun new_collision_object as ^
 {} -> `CollisionObject'
#}

{#fun free_collision_object as ^
 { `CollisionObject' } -> `()'
#}

instance New CollisionObject () where
  new _ = newCollisionObject
  del x = freeCollisionObject x

{#fun co_allocate_world_transform as coAllocateWorldTransform_
 { `CollisionObject' } -> `Transform'
#}

coAllocateWorldTransform :: IsCollisionObject co => co -> IO Transform
coAllocateWorldTransform = coAllocateWorldTransform_ . toCollisionObject

{#fun co_set_world_transform as coSetWorldTransform_
 { `CollisionObject',
   `Transform' } -> `()'
#}

coSetWorldTransform :: IsCollisionObject co => co -> Transform -> IO ()
coSetWorldTransform = coSetWorldTransform_ . toCollisionObject

{#fun is_static_object as isStaticObject_
 { `CollisionObject' } -> `Bool'
#}

isStaticObject :: IsCollisionObject co => co -> IO Bool
isStaticObject = isStaticObject_ . toCollisionObject

{#fun is_kinematic_object as isKinematicObject_
 { `CollisionObject' } -> `Bool'
#}

isKinematicObject :: IsCollisionObject co => co -> IO Bool
isKinematicObject = isKinematicObject_ . toCollisionObject

{#fun is_static_or_kinematic_object as isStaticOrKinematicObject_
 { `CollisionObject' } -> `Bool'
#}

isStaticOrKinematicObject :: IsCollisionObject co => co -> IO Bool
isStaticOrKinematicObject = isStaticOrKinematicObject_ . toCollisionObject

{#fun has_contact_response as hasContactResponse_
 { `CollisionObject' } -> `Bool'
#}

hasContactResponse :: IsCollisionObject co => co -> IO Bool
hasContactResponse = hasContactResponse_ . toCollisionObject

{#fun get_collision_shape as getCollisionShape_
 { `CollisionObject' } -> `CollisionShape'
#}

getCollisionShape :: IsCollisionObject co => co -> IO CollisionShape
getCollisionShape = getCollisionShape_ . toCollisionObject

{#fun set_collision_shape as setCollisionShape_
 { `CollisionObject',
   `CollisionShape' } -> `()'
#}

setCollisionShape :: (IsCollisionShape cs, IsCollisionObject co) => co -> cs -> IO ()
setCollisionShape co cs = setCollisionShape_ (toCollisionObject co) (toCollisionShape cs)

{#fun set_activation_state as ^
 { `CollisionObject',
   `Int' } -> `()'
#}

{#fun get_interpolation_linear_velocity as ^
 { `CollisionObject',
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek* } -> `()'
#}

{#fun set_interpolation_linear_velocity as ^
 { `CollisionObject',
   `CFloat',
   `CFloat',
   `CFloat' } -> `()'
#}

{#fun set_user_index as setUserIndex_
 { `CollisionObject',
   `Int' } -> `()'
#}

setUserIndex :: IsCollisionObject co => co -> Int -> IO ()
setUserIndex = setUserIndex_ . toCollisionObject

{#fun get_user_index as getUserIndex_
 { `CollisionObject' } -> `Int'
#}

getUserIndex :: IsCollisionObject co => co -> IO Int
getUserIndex = getUserIndex_ . toCollisionObject

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
{#fun new_rigid_body_construction_info as newRigidBodyConstructionInfo_
 { `CFloat',
   `MotionState',
   `CollisionShape',
   `CFloat',
   `CFloat',
   `CFloat' } -> `RigidBodyConstructionInfo'
#}

newRigidBodyConstructionInfo :: IsCollisionShape cs => CFloat -> MotionState -> cs -> CFloat -> CFloat -> CFloat -> IO RigidBodyConstructionInfo
newRigidBodyConstructionInfo m ms cs x y z = newRigidBodyConstructionInfo_ m ms (toCollisionShape cs) x y z

{#fun free_rigid_body_construction_info as ^
 { `RigidBodyConstructionInfo' } -> `()'
#}

instance New RigidBodyConstructionInfo (Mass, MotionState, CollisionShape, (CFloat, CFloat, CFloat)) where
  new (m, ms, cs, (x, y, z)) = newRigidBodyConstructionInfo m ms cs x y z
  del x = freeRigidBodyConstructionInfo x

-- | btRigidBody
instance IsCollisionObject RigidBody

{#fun new_rigid_body as ^
 { `RigidBodyConstructionInfo' } -> `RigidBody'
#}

{#fun free_rigid_body as ^
 { `RigidBody' } -> `()'
#}

{#fun rb_get_motion_state as ^
 { `RigidBody' } -> `MotionState'
#}

{#fun rb_set_gravity as ^
 { `RigidBody',
   `CFloat',
   `CFloat',
   `CFloat' } -> `()'
#}

{#fun rb_get_gravity as ^
 { `RigidBody',
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek* } -> `()'
#}

{#fun rb_set_linear_velocity as ^
 { `RigidBody',
   `CFloat',
   `CFloat',
   `CFloat' } -> `()'
#}

{#fun rb_get_linear_velocity as ^
 { `RigidBody',
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek* } -> `()'
#}


{#fun get_total_force as ^
 { `RigidBody',
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek* } -> `()'
#}

{#fun get_total_torque as ^
 { `RigidBody',
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek* } -> `()'
#}

{#fun apply_force as ^
 { `RigidBody',
   `CFloat',
   `CFloat',
   `CFloat',
   `CFloat',
   `CFloat',
   `CFloat' } -> `()'
#}

{#fun apply_torque as ^
 { `RigidBody',
   `CFloat',
   `CFloat',
   `CFloat' } -> `()'
#}

{#fun clear_forces as ^
 { `RigidBody' } -> `()'
#}

{#fun allocate_center_of_mass_transform as ^
 { `RigidBody' } -> `Transform'
#}

-- | btConvexShape
class IsConvexShape cs where
  toConvexShape :: cs -> ConvexShape
  toConvexShape = unsafeCoerce

instance IsConvexShape ConvexShape

-- | btCollisionShape
class IsCollisionShape cs where
  toCollisionShape :: cs -> CollisionShape
  toCollisionShape = unsafeCoerce

instance IsCollisionShape CollisionShape

{#fun free_collision_shape as freeCollisionShape_
 { `CollisionShape' } -> `()'
#}

freeCollisionShape :: IsCollisionShape cs => cs -> IO ()
freeCollisionShape = freeCollisionShape_ . toCollisionShape

{#fun calculate_local_inertia as calculateLocalInertia_
 { `CollisionShape',
   `CFloat',
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek* } -> `()'
#}

calculateLocalInertia :: IsCollisionShape cs => cs -> Mass -> IO (CFloat, CFloat, CFloat)
calculateLocalInertia = calculateLocalInertia_ . toCollisionShape

{#fun get_bounding_sphere as getBoundingSphere_
 { `CollisionShape',
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek* } -> `()'
#}

getBoundingSphere :: IsCollisionShape cs => cs -> IO (CFloat, CFloat, CFloat, CFloat)
getBoundingSphere = getBoundingSphere_ . toCollisionShape

{#fun is_convex as isConvex_
 { `CollisionShape' } -> `Bool'
#}

isConvex :: IsCollisionShape cs => cs -> IO Bool
isConvex = isConvex_ . toCollisionShape

{#fun is_polyhedral as isPolyhedral_
 { `CollisionShape' } -> `Bool'
#}

isPolyhedral :: IsCollisionShape cs => cs -> IO Bool
isPolyhedral = isPolyhedral_ . toCollisionShape

{#fun is_non_moving as isNonMoving_
 { `CollisionShape' } -> `Bool'
#}

isNonMoving :: IsCollisionShape cs => cs -> IO Bool
isNonMoving = isNonMoving_ . toCollisionShape

{#fun is_concave as isConcave_
 { `CollisionShape' } -> `Bool'
#}

isConcave :: IsCollisionShape cs => cs -> IO Bool
isConcave = isConcave_ . toCollisionShape

{#fun is_compound as isCompound_
 { `CollisionShape' } -> `Bool'
#}

isCompound :: IsCollisionShape cs => cs -> IO Bool
isCompound = isCompound_ . toCollisionShape

{#fun is_soft_body as isSoftBody_
 { `CollisionShape' } -> `Bool'
#}

isSoftBody :: IsCollisionShape cs => cs -> IO Bool
isSoftBody = isSoftBody_ . toCollisionShape

{#fun is_infinite as isInfinite_
 { `CollisionShape' } -> `Bool'
#}

isInfinite :: IsCollisionShape cs => cs -> IO Bool
isInfinite = isInfinite_ . toCollisionShape

-- | btStaticPlaneShape
instance IsCollisionShape StaticPlaneShape

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
instance IsCollisionShape CapsuleShape
instance IsConvexShape CapsuleShape

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

-- | btSphereShape
instance IsCollisionShape SphereShape
instance IsConvexShape SphereShape

{#fun new_sphere_shape as ^
 { `CFloat' } -> `SphereShape'
#}

{#fun free_sphere_shape as ^
 { `SphereShape' } -> `()'
#}

instance New SphereShape CFloat where
  new r = newSphereShape r
  del x = freeSphereShape x

-- | btBoxShape
instance IsCollisionShape BoxShape
instance IsConvexShape BoxShape

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
class IsTypedConstraint tc where
  toTypedConstraint :: tc -> TypedConstraint
  toTypedConstraint = unsafeCoerce

{#fun free_typed_constraint as freeTypedConstraint_
 { `TypedConstraint' } -> `()'
#}

freeTypedConstraint :: IsTypedConstraint tc => tc -> IO ()
freeTypedConstraint = freeTypedConstraint_ . toTypedConstraint

-- | btPoint2PointConstraint
instance IsTypedConstraint Point2PointConstraint

{#fun new_point2point_constraint as newPoint2PointConstraint
 { `RigidBody',
   `CFloat',
   `CFloat',
   `CFloat' } -> `Point2PointConstraint'
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
instance IsCollisionObject PairCachingGhostObject

{#fun new_pair_caching_ghost_object as ^
 {} -> `PairCachingGhostObject'
#}

{#fun free_pair_caching_ghost_object as ^
 { `PairCachingGhostObject' } -> `()'
#}

instance New PairCachingGhostObject () where
  new _ = newPairCachingGhostObject
  del x = freePairCachingGhostObject x

-- | btActionInterface
class IsActionInterface ai where
  toActionInterface :: ai -> ActionInterface
  toActionInterface = unsafeCoerce

instance IsActionInterface ActionInterface

-- | btKinematicCharacterController
instance IsActionInterface KinematicCharacterController

{#fun new_kinematic_character_controller as newKinematicCharacterController_
 { `PairCachingGhostObject',
   `ConvexShape',
   `CFloat' } -> `KinematicCharacterController'
#}

newKinematicCharacterController :: IsConvexShape cs => PairCachingGhostObject -> cs -> CFloat -> IO KinematicCharacterController
newKinematicCharacterController pcgo cs sh = newKinematicCharacterController_ pcgo (toConvexShape cs) sh

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

{#fun kcc_set_angular_velocity as ^
 { `KinematicCharacterController',
   `CFloat',
   `CFloat',
   `CFloat' } -> `()'
#}

{#fun kcc_get_angular_velocity as ^
 { `KinematicCharacterController',
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek*,
   alloca- `CFloat' peek* } -> `()'
#}

{#fun kcc_set_linear_velocity as ^
 { `KinematicCharacterController',
   `CFloat',
   `CFloat',
   `CFloat' } -> `()'
#}

{#fun kcc_get_linear_velocity as ^
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
