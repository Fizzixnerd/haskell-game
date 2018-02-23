{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Graphics.Binding.OpenGL.Uniform where

import Graphics.Binding.OpenGL.Utils
import Graphics.Binding.OpenGL.Program

-- Row major!

type DefaultBlock = Program

class Uniform a where
  type UniformContents a
  type UniformLocationType a
  uniform :: (MonadIO m, Storable (UniformContents a)) => UniformLocationType a -> a -> UniformContents a -> m ()
