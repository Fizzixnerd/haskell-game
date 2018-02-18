{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Game.Graphics.Binding.OpenGL.Uniform where

import Data.StateVar
import Game.Graphics.Binding.OpenGL.Utils
import Game.Graphics.Binding.OpenGL.Shader
import Game.Graphics.Binding.OpenGL.PrimUniform
import Graphics.GL.Types

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
