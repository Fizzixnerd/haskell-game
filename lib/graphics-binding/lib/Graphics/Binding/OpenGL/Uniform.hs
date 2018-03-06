{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Binding.OpenGL.Uniform where

import Graphics.GL.Core45
import Graphics.Binding.OpenGL.Utils
import Graphics.Binding.OpenGL.Types
import Graphics.Binding.OpenGL.BufferObject
import Data.Typeable
-- Row major!

bindPersistentBufferToPoint :: MonadIO m => BufferObjectIndex -> PersistentBuffer a -> BufferObjectOffset -> BufferObjectSize -> m ()
bindPersistentBufferToPoint idx (PersistentBuffer _ n _) = bindSizedBufferRange BufferUniform idx n

bindFullPersistentBufferToPoint :: forall a m. (GLWritable a, MonadIO m) => BufferObjectIndex -> PersistentBuffer a -> m ()
bindFullPersistentBufferToPoint idx (PersistentBuffer _ n _) = bindSizedBufferRange BufferUniform idx n 0 (gSize (Proxy :: Proxy a))

bindFullWritableBufferToPoint :: forall a m. (GLWritable a, MonadIO m) => BufferObjectIndex -> WritableBuffer a -> m ()
bindFullWritableBufferToPoint idx (WritableBuffer n) = bindSizedBufferRange BufferUniform idx n 0 (gSize (Proxy :: Proxy a))

uniformBlockBinding :: MonadIO m => Program -> BufferObjectIndex -> BufferObjectIndex -> m ()
uniformBlockBinding (Program a) (BufferObjectIndex b) (BufferObjectIndex c) = glUniformBlockBinding a b c
