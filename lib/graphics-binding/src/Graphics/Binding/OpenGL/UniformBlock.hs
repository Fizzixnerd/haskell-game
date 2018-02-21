{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Binding.OpenGL.UniformBlock where

import Graphics.GL.Types
import Graphics.Binding.OpenGL.Utils
import Graphics.Binding.OpenGL.Program
import Graphics.Binding.OpenGL.PrimUniform
import Data.StateVar

newtype UniformBlock = UniformBlock
  { uniformBlockInternal :: GLuint
  } deriving (Eq, Ord, Show, Storable)

-- Row major!

type DefaultBlock = Program

class Uniform a where
  type (UniformContents a)
  type (UniformLocationType a)
  uniformLocation :: a -> GettableStateVar UniformLocation
  uniform :: Storable (UniformContents a) => UniformLocationType a -> a -> SettableStateVar (UniformContents a)
--  uniformBlock :: Storable (UniformContents a) => UniformBlock -> a -> GettableStateVar (UniformContents a)

class (Uniform a, Storable b) => HasUniformComponent a b where
  uniformComponentDefault :: Program -> a -> SettableStateVar b
--  uniformComponentBlock :: UniformBlock -> a -> SettableStateVar b
