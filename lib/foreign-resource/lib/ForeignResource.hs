{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module ForeignResource
  ( module X
  , ForeignResource(..)
  , resource'
  , newResource
  , newResource'
  , newLongResource
  , newLongResource'
  , withResource
  , withResource'
  , ForeignResourceRead(..)
  , resourceRead
  , resourceRead'
  , ForeignResourceWrite(..)
  , resourceWrite
  , resourceWrite'
  , (!.=)
  , (~&)
  , ForeignResourceUpdate(..)
  , resourceUpdate
  , resourceUpdate'
  , (!%=)
  )
where

import Data.Acquire as X
import Control.Monad.IO.Class as X
import Control.Monad.Trans.Resource as X
import Data.Functor (($>))

class ForeignResource s a | s -> a where
  resource :: a -> Acquire s

resource' :: ForeignResource s () => Acquire s
resource' = resource ()

newResource :: (MonadResource m, ForeignResource s a) => a -> m (ReleaseKey, s)
newResource = allocateAcquire . resource

newResource' :: (MonadResource m, ForeignResource s ()) => m (ReleaseKey, s)
newResource' = newResource ()

newLongResource :: (MonadResource m, ForeignResource s a) => a -> m s
newLongResource = fmap snd . newResource

newLongResource' :: (MonadResource m, ForeignResource s ()) => m s
newLongResource' = newLongResource ()

withResource :: (MonadBaseControl IO m, ForeignResource s a) => a -> (s -> m b) -> m b
withResource a = with (resource a)

withResource' :: (MonadBaseControl IO m, ForeignResource s ()) => (s -> m b) -> m b
withResource' = with resource'

class ForeignResourceRead s t r | t -> r where
  resourceRead_ :: s -> t -> IO r

resourceRead :: (MonadIO m, ForeignResourceRead s t r) => s -> t -> m r
resourceRead s = liftIO . resourceRead_ s

resourceRead' :: (MonadIO m, ForeignResourceRead s () r) => s -> m r
resourceRead' s = resourceRead s ()

class ForeignResourceWrite s t w | t -> w where
  resourceWrite_ :: s -> t -> w -> IO s

resourceWrite :: (MonadIO m, ForeignResourceWrite s t w) => s -> t -> w -> m s
resourceWrite s t = liftIO . resourceWrite_ s t

resourceWrite' :: (MonadIO m, ForeignResourceWrite s () w) => s -> w -> m s
resourceWrite' s = resourceWrite s ()

infix 4 !.=
(!.=) :: (MonadIO m, ForeignResourceWrite s t w) => t -> w -> s -> m s
t !.= w = \s -> resourceWrite s t w

infixl 1 ~&
(~&) :: Functor f => s -> (s -> f s) -> f ()
s ~& f = f s $> ()

class (ForeignResourceRead s t a, ForeignResourceWrite s t a) => ForeignResourceUpdate s t a | t -> a where
  resourceUpdate_ :: t -> (a -> IO a) -> s -> IO s

resourceUpdate :: (MonadIO m, ForeignResourceUpdate s t a) => t -> (a -> IO a) -> s -> m s
resourceUpdate t f = liftIO . resourceUpdate_ t f

resourceUpdate' :: (MonadIO m, ForeignResourceUpdate s () a) => (a -> IO a) -> s -> m s
resourceUpdate' = resourceUpdate ()

infix 4 !%=
(!%=) :: (MonadIO m, ForeignResourceUpdate s t a) => t -> (a -> IO a) -> s -> m s
(!%=) = resourceUpdate

