{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Graphics.Binding.OpenGL.Uniform where

import Graphics.GL.Core45
import Graphics.Binding.OpenGL.Utils
import Graphics.Binding.OpenGL.Program
import Graphics.Binding.OpenGL.ObjectName
import Graphics.Binding.OpenGL.BufferObject
import Foreign.Ptr
-- Row major!

type DefaultBlock = Program

class Uniform a where
  type UniformContents a
  type UniformLocationType a
  uniform :: (MonadIO m, Storable (UniformContents a)) => UniformLocationType a -> a -> UniformContents a -> m ()

uniformBlockBinding :: MonadIO m => Program -> BufferObjectIndex -> BufferObject -> m ()
uniformBlockBinding (Program a) (BufferObjectIndex b) (BufferObject c) = glUniformBlockBinding a b c

persistentUniformBlockBinding :: MonadIO m => Program -> BufferObjectIndex -> PersistentBuffer a -> m ()
persistentUniformBlockBinding prg indx (PersistentBuffer _ n _) = uniformBlockBinding prg indx n

{-
class GeneratableObjectName a => UniformBlock a where
  type UniformBlockContents a
  setUniformBlock_  :: Storable (UniformBlockContents a) => a -> Ptr (UniformBlockContents a) -> IO ()

-- I guess everything is array-ish...
class UniformBlock s => HasUniformComponent s a where
  type ComponentContents a

class Storable a => ForeignReference s a | s -> a where
  getReference_ :: IO s
  setReference_ :: s -> a -> IO ()

class Storable a => GLBufferObject s a | s -> a
-}
