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


data Diffuse2DSampler = Diffuse2DSampler deriving (Eq, Ord, Show)

instance TextureSampler Diffuse2DSampler where
  type TextureSamplerTarget Diffuse2DSampler = TextureTarget2D
  type TextureSamplerType   Diffuse2DSampler = 'SamplerFloat
  texture tex _ = primTextureUnitBind tex 0

data Specular2DSampler = Specular2DSampler deriving (Eq, Ord, Show)

instance TextureSampler Specular2DSampler where
  type TextureSamplerTarget Specular2DSampler = TextureTarget2D
  type TextureSamplerType   Specular2DSampler = 'SamplerFloat
  texture tex _ = primTextureUnitBind tex 1

data Ambient2DSampler = Ambient2DSampler deriving (Eq, Ord, Show)

instance TextureSampler Ambient2DSampler where
  type TextureSamplerTarget Ambient2DSampler = TextureTarget2D
  type TextureSamplerType   Ambient2DSampler = 'SamplerFloat
  texture tex _ = primTextureUnitBind tex 2

data Emmisive2DSampler = Emmisive2DSampler deriving (Eq, Ord, Show)

instance TextureSampler Emmisive2DSampler where
  type TextureSamplerTarget Emmisive2DSampler = TextureTarget2D
  type TextureSamplerType   Emmisive2DSampler = 'SamplerFloat
  texture tex _ = primTextureUnitBind tex 3

data Height2DSampler = Height2DSampler deriving (Eq, Ord, Show)

instance TextureSampler Height2DSampler where
  type TextureSamplerTarget Height2DSampler = TextureTarget2D
  type TextureSamplerType   Height2DSampler = 'SamplerFloat
  texture tex _ = primTextureUnitBind tex 4

data Normal2DSampler = Normal2DSampler deriving (Eq, Ord, Show)

instance TextureSampler Normal2DSampler where
  type TextureSamplerTarget Normal2DSampler = TextureTarget2D
  type TextureSamplerType   Normal2DSampler = 'SamplerFloat
  texture tex _ = primTextureUnitBind tex 5

data Shininess2DSampler = Shininess2DSampler deriving (Eq, Ord, Show)

instance TextureSampler Shininess2DSampler where
  type TextureSamplerTarget Shininess2DSampler = TextureTarget2D
  type TextureSamplerType   Shininess2DSampler = 'SamplerFloat
  texture tex _ = primTextureUnitBind tex 6

data Opacity2DSampler = Opacity2DSampler deriving (Eq, Ord, Show)

instance TextureSampler Opacity2DSampler where
  type TextureSamplerTarget Opacity2DSampler = TextureTarget2D
  type TextureSamplerType   Opacity2DSampler = 'SamplerFloat
  texture tex _ = primTextureUnitBind tex 7

data Displacement2DSampler = Displacement2DSampler deriving (Eq, Ord, Show)

instance TextureSampler Displacement2DSampler where
  type TextureSamplerTarget Displacement2DSampler = TextureTarget2D
  type TextureSamplerType   Displacement2DSampler = 'SamplerFloat
  texture tex _ = primTextureUnitBind tex 8

data LightMap2DSampler = LightMap2DSampler deriving (Eq, Ord, Show)

instance TextureSampler LightMap2DSampler where
  type TextureSamplerTarget LightMap2DSampler = TextureTarget2D
  type TextureSamplerType   LightMap2DSampler = 'SamplerFloat
  texture tex _ = primTextureUnitBind tex 9

data Reflection2DSampler = Reflection2DSampler deriving (Eq, Ord, Show)

instance TextureSampler Reflection2DSampler where
  type TextureSamplerTarget Reflection2DSampler = TextureTarget2D
  type TextureSamplerType   Reflection2DSampler = 'SamplerFloat
  texture tex _ = primTextureUnitBind tex 10
