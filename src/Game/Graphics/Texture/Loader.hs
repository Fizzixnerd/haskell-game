{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Game.Graphics.Texture.Loader where

import           ClassyPrelude
import           Foreign.Ptr
import qualified Codec.Picture                as P
import qualified Data.Vector.Storable         as VS
import           Graphics.Binding
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

loadPNGTexture :: MonadIO m => FilePath -> m (TextureObject TextureTarget2D)
loadPNGTexture fp = liftIO $ do
  pic <- loadPic fp
  case pic of
    (P.ImageRGBA8 (P.Image w h vimg)) -> do
      let attr = Texture2DAttrib SizedRGBA8 1 w h
          pixAttr = Pixel2DAttrib PixelRGBA GLUnsignedByte w h Nothing Nothing
      tobj <- createTexture Texture2D attr
      VS.unsafeWith vimg (textureSubMap tobj 0 pixAttr . castPtr)
      textureParameteri tobj TextureMinFilter GL_NEAREST
      textureParameteri tobj TextureMagFilter GL_NEAREST
      textureParameteri tobj TextureCompareFunc GL_LEQUAL
      return tobj
    (P.ImageRGB8 (P.Image w h vimg)) -> do
      let attr = Texture2DAttrib SizedRGB8 1 w h
          pixAttr = Pixel2DAttrib PixelRGB GLUnsignedByte w h Nothing Nothing
      tobj <- createTexture Texture2D attr
      VS.unsafeWith vimg (textureSubMap tobj 0 pixAttr . castPtr)
      textureParameteri tobj TextureMinFilter GL_NEAREST
      textureParameteri tobj TextureMagFilter GL_NEAREST
      textureParameteri tobj TextureCompareFunc GL_LEQUAL
      return tobj
    

data Simple2DSampler = Simple2DSampler deriving (Eq, Ord, Show)

instance TextureSampler Simple2DSampler where
  type TextureSamplerTarget Simple2DSampler = TextureTarget2D
  type TextureSamplerType   Simple2DSampler = 'SamplerFloat
  texture tex _ = primTextureUnitBind tex 0
