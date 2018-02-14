{-# LANGUAGE NoImplicitPrelude #-}

module Game.Graphics.Texture.Loader where

import           ClassyPrelude
import           Foreign.Ptr
import qualified Codec.Picture                as P
import qualified Data.Vector.Storable         as VS
import           Game.Graphics.OpenGL.Binding
import           Graphics.GL.Core45

loadPic :: MonadIO m => FilePath -> m P.DynamicImage
loadPic fp = do
  bmpObj <- liftIO $ P.readImage fp
  case bmpObj of
    Left e -> error e
    Right obj -> return obj

loadBMPTexture :: MonadIO m => FilePath -> m (TextureObject TextureTarget2D)
loadBMPTexture fp = liftIO $ do
  (P.ImageRGB8 (P.Image w h vimg)) <- loadPic fp
  let attr = Texture2DAttrib SizedRGB8 1 w h
      pixAttr = Pixel2DAttrib PixelRGB GLUnsignedByte w h Nothing Nothing
  tobj <- createTexture Texture2D attr
  VS.unsafeWith vimg (textureSubMap tobj 0 pixAttr . castPtr)
  textureParameteri tobj TextureMinFilter GL_NEAREST
  textureParameteri tobj TextureMagFilter GL_NEAREST
  textureParameteri tobj TextureCompareFunc GL_LEQUAL
  return tobj

