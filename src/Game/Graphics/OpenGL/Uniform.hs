{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Graphics.OpenGL.Uniform where

import Graphics.GL.Core45
import Graphics.GL.Types
import Game.Graphics.OpenGL.Utils
import Game.Graphics.OpenGL.DataType
import Game.Graphics.OpenGL.Shader
import Game.Graphics.OpenGL.BufferObject
import Game.Graphics.OpenGL.PrimUniform
import Data.StateVar
import Linear
import qualified Data.Vector.Storable as VS

newtype UniformBlock = UniformBlock
  { uniformBlockInternal :: GLuint
  } deriving (Eq, Ord, Show, Storable)

-- Row major!

class Uniform a where
  type (UniformContents a)
  uniformLocation :: a -> GettableStateVar UniformLocation
  uniformDefault :: Storable (UniformContents a) => Program -> a -> SettableStateVar (UniformContents a)
--  uniformBlock :: Storable (UniformContents a) => UniformBlock -> a -> GettableStateVar (UniformContents a)

class (Uniform a, Storable b) => HasUniformComponent a b where
  uniformComponentDefault :: Program -> a -> SettableStateVar b
--  uniformComponentBlock :: UniformBlock -> a -> SettableStateVar b

{-
setUniformRowMajor :: MonadIO m => Program -> UniformLocation -> m ()
setUniformRowMajor (Program n) (UniformLocation loc) = glGetProgramResourceiv n UNIFORM (fromIntegral loc) 0 nullPtr 0 nullPtr GL_IS_ROW_MAJOR
-}
