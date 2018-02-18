{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Game.Graphics.Binding.OpenGL.Texture where

import Game.Graphics.Binding.OpenGL.Utils
import Game.Graphics.Binding.OpenGL.ObjectName
import Game.Graphics.Binding.OpenGL.Boolean
import Data.Vector.Storable as VS
import Graphics.GL.Core45
import Graphics.GL.Types
import Control.Lens
import Game.Graphics.Binding.OpenGL.DataType

newtype TextureUnit = TextureUnit
  { getTextureUnitGLuint :: GLuint
  } deriving (Eq, Ord, Show)

data TextureTarget1D
  = Texture1D
  deriving (Eq, Ord, Show)

data TextureTarget2D
  = Texture2D
  | TextureRectangle
  | TextureCubeMap
  | Texture1DArray
  deriving (Eq, Ord, Show)

data TextureTarget3D
  = Texture3D
  | Texture2DArray
  | TextureCubeMapArray
  deriving (Eq, Ord, Show)

data Texture2DMultisample = Texture2DMultisample
  deriving (Eq, Ord, Show)

data Texture2DMultisampleArray = Texture2DMultisampleArray
  deriving (Eq, Ord, Show)

data Texture1DAttrib = Texture1DAttrib
  { _texture1DAttribBufferFormat :: SizedFormat
  , _texture1DAttribMipmapLevel  :: Int
  , _texture1DAttribBufferWidth  :: Int
  } deriving (Eq, Ord, Show)

data Texture2DAttrib = Texture2DAttrib
  { _texture2DAttribBufferFormat :: SizedFormat
  , _texture2DAttribMipmapLevel  :: Int
  , _texture2DAttribBufferWidth  :: Int
  , _texture2DAttribBufferHeight :: Int
  } deriving (Eq, Ord, Show)

data Texture3DAttrib = Texture3DAttrib
  { _texture3DAttribBufferFormat :: SizedFormat
  , _texture3DAttribMipmapLevel  :: Int
  , _texture3DAttribBufferWidth  :: Int
  , _texture3DAttribBufferHeight :: Int
  , _texture3DAttribBufferDepth  :: Int
  } deriving (Eq, Ord, Show)

newtype TextureObject t = TextureObject
  { textureObjectInternal :: GLuint
  } deriving (Eq, Ord, Show, Storable)

instance ObjectName (TextureObject t) where
  isObjectName (TextureObject n) = unmarshallGLboolean <$> glIsTexture n
  deleteObjectNames ns = liftIO . VS.unsafeWith ns $ \ptr -> glDeleteTextures len (castPtr ptr)
    where
      len = fromIntegral $ VS.length ns

data PixelFormat
   = PixelStencilIndex
   | PixelDepthStencil
   | PixelDepthComponent
   | PixelRed
   | PixelGreen
   | PixelBlue
   | PixelRG
   | PixelRGB
   | PixelRGBA
   | PixelRedInteger
   | PixelGreenInteger
   | PixelBlueInteger
   | PixelRGInteger
   | PixelRGBInteger
   | PixelRGBAInteger
   | PixelBGRInteger
   | PixelBGRAInteger
   | PixelBGR
   | PixelBGRA
   deriving ( Eq, Ord, Show )

data Pixel1DAttrib = Pixel1DAttrib
  { _pixel1DAttribPixelFormat  :: PixelFormat
  , _pixel1DAttribPixelType    :: GLDataType
  , _pixel1DAttribPixelWidth   :: Int
  , _pixel1DAttribPixelXOffset :: Maybe Int
  } deriving (Eq, Ord, Show)

data Pixel2DAttrib = Pixel2DAttrib
  { _pixel2DAttribPixelFormat  :: PixelFormat
  , _pixel2DAttribPixelType    :: GLDataType
  , _pixel2DAttribPixelWidth   :: Int
  , _pixel2DAttribPixelHeight  :: Int
  , _pixel2DAttribPixelXOffset :: Maybe Int
  , _pixel2DAttribPixelYOffset :: Maybe Int
  } deriving (Eq, Ord, Show)


data Pixel3DAttrib = Pixel3DAttrib
  { _pixel3DAttribPixelFormat  :: PixelFormat
  , _pixel3DAttribPixelType    :: GLDataType
  , _pixel3DAttribPixelWidth   :: Int
  , _pixel3DAttribPixelHeight  :: Int
  , _pixel3DAttribPixelDepth   :: Int
  , _pixel3DAttribPixelXOffset :: Maybe Int
  , _pixel3DAttribPixelYOffset :: Maybe Int
  , _pixel3DAttribPixelZOffset :: Maybe Int
  } deriving (Eq, Ord, Show)

makeFields ''Texture1DAttrib
makeFields ''Texture2DAttrib
makeFields ''Texture3DAttrib
makeFields ''Pixel1DAttrib
makeFields ''Pixel2DAttrib
makeFields ''Pixel3DAttrib

marshallPixelFormat :: PixelFormat -> GLenum
marshallPixelFormat = \case
  PixelStencilIndex -> GL_STENCIL_INDEX
  PixelDepthComponent -> GL_DEPTH_COMPONENT
  PixelDepthStencil  -> GL_DEPTH_STENCIL
  PixelRed -> GL_RED
  PixelGreen -> GL_GREEN
  PixelBlue -> GL_BLUE
  PixelRG -> GL_RG
  PixelRGB -> GL_RGB
  PixelBGR -> GL_BGR
  PixelRGBA -> GL_RGBA
  PixelBGRA -> GL_BGRA
  PixelRedInteger -> GL_RED_INTEGER
  PixelGreenInteger -> GL_GREEN_INTEGER
  PixelBlueInteger -> GL_BLUE_INTEGER
  PixelRGInteger -> GL_RG_INTEGER
  PixelRGBInteger -> GL_RGB_INTEGER
  PixelBGRInteger -> GL_BGR_INTEGER
  PixelRGBAInteger -> GL_RGBA_INTEGER
  PixelBGRAInteger -> GL_BGRA_INTEGER

data TextureParameter =
     TextureMinFilter
   | TextureMagFilter
   | TextureWrapS
   | TextureWrapT
   | TextureWrapR
   | TextureBorderColor
   | TextureMinLOD
   | TextureMaxLOD
   | TextureBaseLevel
   | TextureMaxLevel
   | TextureCompareMode
   | TextureCompareFunc
   | TextureLODBias
   deriving (Eq, Ord, Show)


class TextureTarget t where
  type TextureConfig t
  type PixelConfig t
  marshallTextureTarget :: t -> GLenum
  createTexture  :: MonadIO m => t -> TextureConfig t -> m (TextureObject t)
  textureSubMap :: MonadIO m => TextureObject t -> Int -> PixelConfig t -> Ptr () -> m ()

genTextureNames :: (MonadIO m, TextureTarget t) => Int -> t -> m (VS.Vector (TextureObject t))
genTextureNames n t = VS.map TextureObject <$> withForeignBufferVec n (glCreateTextures targ (fromIntegral n))
  where
    targ = marshallTextureTarget t

genTextureName :: (MonadIO m, TextureTarget t) => t -> m (TextureObject t)
genTextureName t = VS.head <$> genTextureNames 1 t

instance TextureTarget TextureTarget1D where
  type TextureConfig TextureTarget1D = Texture1DAttrib

  type PixelConfig TextureTarget1D = Pixel1DAttrib

  marshallTextureTarget _ = GL_TEXTURE_1D

  createTexture t Texture1DAttrib {..} = do
    tobj@(TextureObject n) <- genTextureName t
    glTextureStorage1D n (fromIntegral _texture1DAttribMipmapLevel) (marshallSizedFormat _texture1DAttribBufferFormat) (fromIntegral _texture1DAttribBufferWidth)
    return tobj

  textureSubMap (TextureObject tobj) level Pixel1DAttrib {..} = glTextureSubImage1D tobj lev xo width pixForm datType
    where
      xo = maybe 0 fromIntegral _pixel1DAttribPixelXOffset
      lev = fromIntegral level
      pixForm = marshallPixelFormat _pixel1DAttribPixelFormat
      datType = marshallGLDataType _pixel1DAttribPixelType
      width  = fromIntegral _pixel1DAttribPixelWidth

instance TextureTarget TextureTarget2D where
  type TextureConfig TextureTarget2D = Texture2DAttrib

  type PixelConfig TextureTarget2D = Pixel2DAttrib

  marshallTextureTarget = \case
    Texture2D        -> GL_TEXTURE_2D
    TextureRectangle -> GL_TEXTURE_RECTANGLE
    TextureCubeMap   -> GL_TEXTURE_CUBE_MAP
    Texture1DArray   -> GL_TEXTURE_1D_ARRAY

  createTexture t Texture2DAttrib {..} = do
    tobj@(TextureObject n) <- genTextureName t
    glTextureStorage2D n (fromIntegral _texture2DAttribMipmapLevel) (marshallSizedFormat _texture2DAttribBufferFormat) (fromIntegral _texture2DAttribBufferWidth) (fromIntegral _texture2DAttribBufferHeight)
    return tobj

  textureSubMap (TextureObject tobj) level Pixel2DAttrib {..} =  glTextureSubImage2D tobj lev xo yo width height pixForm datType
    where
      xo = maybe 0 fromIntegral _pixel2DAttribPixelXOffset
      yo = maybe 0 fromIntegral _pixel2DAttribPixelYOffset
      lev = fromIntegral level
      pixForm = marshallPixelFormat _pixel2DAttribPixelFormat
      datType = marshallGLDataType _pixel2DAttribPixelType
      width  = fromIntegral _pixel2DAttribPixelWidth
      height = fromIntegral _pixel2DAttribPixelHeight

instance TextureTarget TextureTarget3D where
  type TextureConfig TextureTarget3D = Texture3DAttrib

  type PixelConfig TextureTarget3D = Pixel3DAttrib

  marshallTextureTarget = \case
    Texture3D           -> GL_TEXTURE_3D
    Texture2DArray      -> GL_TEXTURE_2D_ARRAY
    TextureCubeMapArray -> GL_TEXTURE_CUBE_MAP_ARRAY

  createTexture t Texture3DAttrib {..} = do
    tobj@(TextureObject n) <- genTextureName t
    glTextureStorage3D n (fromIntegral _texture3DAttribMipmapLevel) (marshallSizedFormat _texture3DAttribBufferFormat) (fromIntegral _texture3DAttribBufferWidth) (fromIntegral _texture3DAttribBufferHeight) (fromIntegral _texture3DAttribBufferDepth)
    return tobj

  textureSubMap (TextureObject tobj) level Pixel3DAttrib {..} = glTextureSubImage3D tobj lev xo yo zo width height depth pixForm datType
    where
      xo = maybe 0 fromIntegral _pixel3DAttribPixelXOffset
      yo = maybe 0 fromIntegral _pixel3DAttribPixelYOffset
      zo = maybe 0 fromIntegral _pixel3DAttribPixelZOffset
      lev = fromIntegral level
      pixForm = marshallPixelFormat _pixel3DAttribPixelFormat
      datType = marshallGLDataType _pixel3DAttribPixelType
      width  = fromIntegral _pixel3DAttribPixelWidth
      height = fromIntegral _pixel3DAttribPixelHeight
      depth  = fromIntegral _pixel3DAttribPixelDepth


bindTextureUnit :: MonadIO m => TextureUnit -> (TextureObject t) -> m ()
bindTextureUnit (TextureUnit n) (TextureObject m) = glBindTextureUnit n m


marshallTextureParameter :: TextureParameter -> GLenum
marshallTextureParameter = \case
  TextureMinFilter -> GL_TEXTURE_MIN_FILTER
  TextureMagFilter -> GL_TEXTURE_MAG_FILTER
  TextureWrapS -> GL_TEXTURE_WRAP_S
  TextureWrapT -> GL_TEXTURE_WRAP_T
  TextureWrapR -> GL_TEXTURE_WRAP_R
  TextureBorderColor -> GL_TEXTURE_BORDER_COLOR
  TextureMinLOD -> GL_TEXTURE_MIN_LOD
  TextureMaxLOD -> GL_TEXTURE_MAX_LOD
  TextureBaseLevel -> GL_TEXTURE_BASE_LEVEL
  TextureMaxLevel -> GL_TEXTURE_MAX_LEVEL
  TextureCompareMode -> GL_TEXTURE_COMPARE_MODE
  TextureCompareFunc -> GL_TEXTURE_COMPARE_FUNC
  TextureLODBias -> GL_TEXTURE_LOD_BIAS

textureParameterf :: MonadIO m => TextureObject t -> TextureParameter -> GLfloat -> m ()
textureParameterf (TextureObject tobj) param =
  glTextureParameterf tobj (marshallTextureParameter param)

textureParameteri :: MonadIO m => TextureObject t -> TextureParameter -> GLint -> m ()
textureParameteri (TextureObject tobj) param =
  glTextureParameteri tobj (marshallTextureParameter param)
