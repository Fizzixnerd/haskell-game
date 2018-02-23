{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.StateVar where

import ClassyPrelude
import Data.Acquire
import Control.Monad.Trans.Resource
import Control.Monad.State.Strict

-- | StateVars are just Acquire a.

-- Resource layer:
--- Use Acquire a to get resource acquisition and freeing in one place, with mkAcquire
--- Either do allocateAquire to get a persistent resource that can be freed, or
--- with to do a local bracket-y thing.

--- 

infixr 2 $=
class HasSetter t a | t -> a where
  -- | Write a new value into a state variable.
  ($=) :: MonadIO m => t -> a -> m t

newtype StateVar a = StateVar { getInternalStateVar :: Acquire a }

{-
type StateAction b a = State b IO a


data StateVar a = StateVar (IO a) (a -> IO ())

makeStateVar :: IO a -> (a -> IO ()) -> StateVar a
makeStateVar = StateVar
-}
