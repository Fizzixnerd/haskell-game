{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Game.Graphics.Binding.OpenGL.Uniform where

import Data.StateVar
import Game.Graphics.Binding.OpenGL.BufferObject
import Game.Graphics.Binding.OpenGL.ObjectName
import Foreign.Storable
-- Row major!

newtype UniformBlock = UniformBlock
  { getUniformBufferObject :: BufferObject
  } deriving (Eq, Show, Ord, Storable, ObjectName, GeneratableObjectName)

class UniformBlockLike a where


class HasUniformVariable a b where
  type (UniformContents a)
  uniform :: a -> b -> SettableStateVar (UniformContents a)
