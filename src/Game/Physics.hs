{-# LANGUAGE FlexibleInstances #-}
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
import Control.Lens
import Control.Lens.Internal.Setter

type GetM m s a = forall b. (a -> m b) -> (s -> m b)
--type Set' m s a = forall b. (b -> m a) -> (b -> s -> m s)
type SetM m s a = forall b. (s -> m a) -> (s -> m b) -> (s -> m b)
type UpdateM m s a = forall b. (a -> m a) -> (s -> m b) -> (s -> m b)

mkGetM :: Monad m => (s -> m a) -> GetM m s a
mkGetM = (>=>)

mkSetM :: Monad m => (s -> a -> m s) -> SetM m s a
mkSetM set_ obtain_ f s = obtain_ s >>= set_ s >>= f

mkUpdateM :: Monad m => (s -> m a) -> (s -> a -> m s) -> UpdateM m s a
mkUpdateM get_ set_ update_ f s = get_ s >>= update_ >>= set_ s >>= f

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

{-
(^.!) :: GettableM m s a t => s -> a -> m t
(^.!)  = flip $ flip getM return

useM :: GettableM m s a t => s -> a -> (t -> m b) -> m b
useM s a f = s ^.! a >>= f

(&!) :: Monad m => s -> (s -> m b) -> m ()
s &! f = void . f $ s
-}


-- a -> f (m a)
-- s -> f (m s)
type OnlySetter m s a = Setter s (m s) () a

makeOnlySetter :: Monad m => (s -> a -> m s) -> OnlySetter m s a
makeOnlySetter set_ = taintedDot . flip set_ . flip untaintedDot ()

class (Traversable f, Monad m) => Alien f m where
  naturalize :: m (f a) -> f (m a)

naturalJoin :: Alien f m => m (f (m a)) -> f (m a)
naturalJoin = fmap join . naturalize

instance Monad m => Alien Identity m where
  naturalize = Identity . fmap runIdentity

instance Monad m => Alien (Const (m a)) m where
  naturalize = Const . join . fmap getConst

type LensM m s t a b = forall f. Alien f m => (a -> f (m b)) -> s -> f (m t)

lensM :: Monad m => (s -> m a) -> (s -> a -> m s) -> LensM m s s a a
lensM get_ set_ f s = fmap ((set_ s) =<<) $ naturalJoin (f <$> get_ s)

(^.!) :: Monad m => s -> LensM m s s a a -> m a
(^.!) s l = getConst $ l (Const . return) s
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
