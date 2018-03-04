{-# LANGUAGE NoImplicitPrelude #-}

module Graphics.Binding.OpenGL.Synchro where

import Graphics.GL.Core45
import Graphics.GL.Types

import ClassyPrelude
import Foreign.Ptr

lockGLFence :: MonadIO m => GLsync -> m GLsync
lockGLFence sync = when (nullPtr /= sync) (glDeleteSync sync) >> glFenceSync GL_SYNC_GPU_COMMANDS_COMPLETE 0

waitGLFence :: MonadIO m => Word64 -> GLsync -> m ()
waitGLFence n sync = when (nullPtr /= sync) go
  where
    cond x = x == GL_ALREADY_SIGNALED || x == GL_CONDITION_SATISFIED
    go = unlessM (cond <$> glClientWaitSync sync GL_SYNC_FLUSH_COMMANDS_BIT n) go
