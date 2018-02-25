{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.StateVar where

import ClassyPrelude
import Data.Acquire
import Control.Monad.Trans.Resource
import Control.Monad.State.Strict
import Control.Lens

-- Resource layer:
--- Use Acquire a to get resource acquisition and freeing in one place, with mkAcquire
--- Either do allocateAquire to get a persistent resource that can be freed, or
--- with to do a local bracket-y thing.

--- Outer monad is a type ResIO

class (Monad m, Functor f, Contravariant f) => Effective m r f where
  effective :: m r -> f a
  ineffective :: f a -> m r

infixr 2 $=
class HasSetter t a | t -> a where
  ($=) :: MonadIO m => t -> a -> m t

type Stately x a = x -> Acquire a

newly :: MonadResource m
type MutableLensM  m s t a b
    = forall f. (Traversable f) => (a -> f b) -> (s -> m (f t))

type MutableLensM' m s a
    = MutableLensM m s s a a

mkLensM :: (Monad m) => (s -> m a) -> (s -> b -> m t)
        -> MutableLensM m s t a b
mkLensM g s f x = g x >>= traverse (s x) . f

mget :: (Monad m) => MutableLensM m s t a b -> s -> m a
mget l s = getConst <$> l Const s

mset :: Functor m => MutableLensM m s t a b -> s -> b -> m t
mset l s v = runIdentity <$> l (const $ Identity v) s

-- given applicative f and traversable m, we can do
-- m (f a) -> f (m a)
--
{-
type StateAction b a = State b IO a


data StateVar a = StateVar (IO a) (a -> IO ())

makeStateVar :: IO a -> (a -> IO ()) -> StateVar a
makeStateVar = StateVar
-}
