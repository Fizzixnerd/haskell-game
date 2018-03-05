{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Binding.OpenGL.Uniform where

import Graphics.GL.Core45
import Graphics.Binding.OpenGL.Utils
import Graphics.Binding.OpenGL.Program
import Graphics.Binding.OpenGL.ObjectName
import Graphics.Binding.OpenGL.BufferObject
import Graphics.Binding.OpenGL.DataType
import Foreign.Ptr
-- Row major!

type DefaultBlock = Program

class Uniform a where
  type UniformContents a
  type UniformLocationType a
  uniform :: (MonadIO m, Storable (UniformContents a)) => UniformLocationType a -> a -> UniformContents a -> m ()

class UniformBlock a b where
  bindBlock_ :: a -> b -> IO ()

bindBlock :: (UniformBlock a b, MonadIO m) => a -> b -> m ()
bindBlock a = liftIO . bindBlock_ a

bindPersistentBufferToPoint :: MonadIO m => BufferObjectIndex -> PersistentBuffer a -> BufferObjectOffset -> BufferObjectSize -> m ()
bindPersistentBufferToPoint idx (PersistentBuffer _ _ n _) = bindBufferRange BufferUniform idx n

bindFullPersistentBufferToPoint :: forall a m. (Storable a, MonadIO m) => BufferObjectIndex -> PersistentBuffer a -> m ()
bindFullPersistentBufferToPoint idx (PersistentBuffer _ len n _) = bindBufferRange BufferUniform idx n 0 (fromIntegral $ len * sizeOf (error "unreachable" :: a))

uniformBlockBinding :: MonadIO m => Program -> BufferObjectIndex -> BufferObject -> m ()
uniformBlockBinding (Program a) (BufferObjectIndex b) (BufferObject c) = glUniformBlockBinding a b c

persistentUniformBlockBinding :: MonadIO m => Program -> BufferObjectIndex -> PersistentBuffer a -> m ()
persistentUniformBlockBinding prg indx (PersistentBuffer _ _ n _) = uniformBlockBinding prg indx n

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
