{-# LANGUAGE NoImplicitPrelude #-}

module Game.Physics where

import ClassyPrelude
import ForeignResource

-- Example: persistent uniform buffers in OpenGL. They can be attached to binding points (so those binding points are foreign resources to which we can write uniform buffer names), and they have content that we can update.

{--
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
--}
