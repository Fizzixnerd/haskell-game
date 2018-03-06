{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Graphics.Binding.OpenGL.Texture where

import Graphics.Binding.OpenGL.Types
import Graphics.Binding.OpenGL.Utils
import Graphics.GL.Core45
import Graphics.GL.Types

textureParameterf :: MonadIO m => TextureObject t -> TextureParameter -> GLfloat -> m ()
textureParameterf (TextureObject tobj) param =
  glTextureParameterf tobj (marshalTextureParameter param)

textureParameteri :: MonadIO m => TextureObject t -> TextureParameter -> GLint -> m ()
textureParameteri (TextureObject tobj) param =
  glTextureParameteri tobj (marshalTextureParameter param)

primTextureUnitBind_ :: MonadIO m => TextureUnit -> TextureObject t -> m ()
primTextureUnitBind_ (TextureUnit n) (TextureObject m) = glBindTextureUnit n m

primTextureUnitBind :: (TextureTarget t, MonadIO m) => TextureObject t -> TextureUnit -> m ()
primTextureUnitBind = flip primTextureUnitBind_
