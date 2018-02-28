{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Game.Physics where

import ClassyPrelude
import Physics.Bullet
import Control.Monad.Trans.Resource
import Data.Acquire

type GetM m s a = forall b. (a -> m b) -> (s -> m b)
--type Set' m s a = forall b. (b -> m a) -> (b -> s -> m s)
type SetM m s a = forall b. (s -> m a) -> (s -> m b) -> (s -> m b)
type UpdateM m s a = forall b. (a -> m a) -> (s -> m b) -> (s -> m b)

mkGetM :: Monad m => (s -> m a) -> GetM m s a
mkGetM = (>=>)

mkSetM :: Monad m => (s -> a -> m ()) -> SetM m s a
mkSetM set_ obtain_ f s = (obtain_ s >>= set_ s) >> f s

mkUpdateM :: Monad m => (s -> m a) -> (s -> a -> m ()) -> UpdateM m s a
mkUpdateM get_ set_ update_ f s = get_ s >>= update_ >>= set_ s >> f s

class ForeignObject a where
  type ForeignObjectConf a
  resource :: ForeignObjectConf a -> Acquire a

class Monad m => SettableM m s a t | a -> t where
  setM :: a -> SetM m s t

class Monad m => GettableM m s a t | a -> t where
  getM :: a -> GetM m s t

class (Monad m, SettableM m s a t, GettableM m s a t) => UpdatableM m s a t | a -> t where
  updateM :: a -> UpdateM m s t
  updateM x = setM x . getM x

(.=!) :: SettableM m s a t => a -> t -> (s -> m b) -> (s -> m b)
(.=!) targ = setM targ . const . pure

(%=!) :: UpdatableM m s a t => a -> (t -> m t) -> (s -> m b) -> (s -> m b)
(%=!) = updateM

(^.!) :: GettableM m s a t => s -> a -> m t
(^.!)  = flip $ flip getM return

useM :: GettableM m s a t => s -> a -> (t -> m b) -> m b
useM s a f = s ^.! a >>= f

(&!) :: Monad m => s -> (s -> m b) -> m ()
s &! f = void . f $ s

-- |

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
