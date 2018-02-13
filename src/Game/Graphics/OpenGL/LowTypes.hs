{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Game.Graphics.OpenGL.LowTypes where

import Graphics.GL.Types
import Graphics.GL.Core45

import ClassyPrelude
import Control.Lens
import Data.Bits
import Foreign.Marshal.Array

newtype BitAnd a = BitAnd { getBitAnd :: a } deriving (Eq, Ord, Show)
newtype BitOr a = BitOr { getBitOr :: a } deriving (Eq, Ord, Show)

instance (Bits a) => Monoid (BitAnd a) where
  mempty = BitAnd zeroBits
  mappend x y = BitAnd $ getBitAnd x .&. getBitAnd y

instance (Bits a) => Monoid (BitOr a) where
  mempty = BitOr zeroBits
  mappend x y = BitOr $ getBitOr x .&. getBitOr y

newtype BufferObjectSize = BufferObjectSize { _bufferObjectSizeObjectSize :: GLsizeiptr } deriving (Eq, Ord, Show)

newtype BufferObjectOffset = BufferObjectOffset { _bufferObjectOffsetObjectOffset :: GLintptr } deriving (Eq, Ord, Show)

newtype BufferObjectRelOffset = BufferObjectRelOffset { _bufferObjectRelOffsetObjectRelOffset :: GLuint } deriving (Eq, Ord, Show)

newtype BufferObjectStride = BufferObjectStride { _bufferObjectStrideObjectStride :: GLsizei } deriving (Eq, Ord, Show)

newtype BufferObjectComponentSize = BufferObjectComponentSize { _bufferObjectComponentSizeObjectComponentSize :: GLint }

makeFields ''BufferObjectSize
makeFields ''BufferObjectOffset
makeFields ''BufferObjectRelOffset
makeFields ''BufferObjectStride
makeFields ''BufferObjectComponentSize


data IntegerHandling = Normalized | NotNormalized deriving (Eq, Ord, Show)

data BufferObjectMapType = MapNone | MapRead | MapWrite | MapReadWrite
  deriving (Eq, Ord, Show)

data ComponentNumber = ComponentOne | ComponentTwo | ComponentThree | ComponentFour

data BufferObjectAttribFlags = BufferObjectAttribFlags
  { _bufferObjectAttribFlagsMapType :: BufferObjectMapType
  , _bufferObjectAttribFlagsMapPersistent :: Bool
  , _bufferObjectAttribFlagsMapCoherent :: Bool
  , _bufferObjectAttribFlagsMapDynamic :: Bool
  , _bufferObjectAttribFlagsClientStorage :: Bool
  } deriving (Eq, Ord, Show)

defaultBufferAttribFlags :: BufferObjectAttribFlags
defaultBufferAttribFlags = BufferObjectAttribFlags
  { _bufferObjectAttribFlagsMapType       = MapNone
  , _bufferObjectAttribFlagsMapPersistent = False
  , _bufferObjectAttribFlagsMapCoherent   = False
  , _bufferObjectAttribFlagsMapDynamic    = False
  , _bufferObjectAttribFlagsClientStorage = False
  }

data BufferObjectMapFlags = BufferObjectMapFlags
  { _bufferObjectMapFlagsMapType :: BufferObjectMapType
  , _bufferObjectMapFlagsMapPersistent :: Bool
  , _bufferObjectMapFlagsMapCoherent :: Bool
  , _bufferObjectMapFlagsMapInvalidateRange :: Bool
  , _bufferObjectMapFlagsMapInvalidateBuffer :: Bool
  , _bufferObjectMapFlagsMapFlushExplicit :: Bool
  , _bufferObjectMapFlagsMapUnsynchronized :: Bool
  } deriving (Eq, Ord, Show)

makeFields ''BufferObjectAttribFlags
makeFields ''BufferObjectMapFlags

data SizedFormat
  = SizedR8
  | SizedR16
  | SizedR16F
  | SizedR32F
  | SizedR8I
  | SizedR16I
  | SizedR32I
  | SizedR8UI
  | SizedR16UI
  | SizedR32UI
  | SizedRG8
  | SizedRG16
  | SizedRG16F
  | SizedRG32F
  | SizedRG8I
  | SizedRG16I
  | SizedRG32I
  | SizedRG8UI
  | SizedRG16UI
  | SizedRG32UI
  | SizedRGB8
  | SizedRGB8UI
  | SizedRGB32F
  | SizedRGB32I
  | SizedRGB32UI
  | SizedRGBA8
  | SizedRGBA16
  | SizedRGBA16F
  | SizedRGBA32F
  | SizedRGBA8I
  | SizedRGBA16I
  | SizedRGBA32I
  | SizedRGBA8UI
  | SizedRGBA16UI
  | SizedRGBA32UI
   deriving ( Eq, Ord, Show )

data GLDataType =
     GLUnsignedByte
   | GLByte
   | GLUnsignedShort
   | GLShort
   | GLUnsignedInt
   | GLInt
   | GLHalfFloat
   | GLFloat
   | GLUnsignedByte332
   | GLUnsignedByte233Rev
   | GLUnsignedShort565
   | GLUnsignedShort565Rev
   | GLUnsignedShort4444
   | GLUnsignedShort4444Rev
   | GLUnsignedShort5551
   | GLUnsignedShort1555Rev
   | GLUnsignedInt8888
   | GLUnsignedInt8888Rev
   | GLUnsignedInt1010102
   | GLUnsignedInt2101010Rev
   | GLUnsignedInt248
   | GLUnsignedInt10f11f11fRev
   | GLUnsignedInt5999Rev
   | GLFloat32UnsignedInt248Rev
   | GLDouble                    -- vertex arrays (EXT_vertex_array, now core)
   deriving ( Eq, Ord, Show )

marshallGLDataType :: GLDataType -> GLenum
marshallGLDataType = \case
  GLUnsignedByte -> GL_UNSIGNED_BYTE
  GLByte -> GL_BYTE
  GLUnsignedShort -> GL_UNSIGNED_SHORT
  GLShort -> GL_SHORT
  GLUnsignedInt -> GL_UNSIGNED_INT
  GLInt -> GL_INT
  GLHalfFloat -> GL_HALF_FLOAT
  GLFloat -> GL_FLOAT
  GLUnsignedByte332 -> GL_UNSIGNED_BYTE_3_3_2
  GLUnsignedByte233Rev -> GL_UNSIGNED_BYTE_2_3_3_REV
  GLUnsignedShort565 -> GL_UNSIGNED_SHORT_5_6_5
  GLUnsignedShort565Rev -> GL_UNSIGNED_SHORT_5_6_5_REV
  GLUnsignedShort4444 -> GL_UNSIGNED_SHORT_4_4_4_4
  GLUnsignedShort4444Rev -> GL_UNSIGNED_SHORT_4_4_4_4_REV
  GLUnsignedShort5551 -> GL_UNSIGNED_SHORT_5_5_5_1
  GLUnsignedShort1555Rev -> GL_UNSIGNED_SHORT_1_5_5_5_REV
  GLUnsignedInt8888 -> GL_UNSIGNED_INT_8_8_8_8
  GLUnsignedInt8888Rev -> GL_UNSIGNED_INT_8_8_8_8_REV
  GLUnsignedInt1010102 -> GL_UNSIGNED_INT_10_10_10_2
  GLUnsignedInt2101010Rev -> GL_UNSIGNED_INT_2_10_10_10_REV
  GLUnsignedInt248 -> GL_UNSIGNED_INT_24_8
  GLUnsignedInt10f11f11fRev -> GL_UNSIGNED_INT_10F_11F_11F_REV
  GLUnsignedInt5999Rev -> GL_UNSIGNED_INT_5_9_9_9_REV
  GLFloat32UnsignedInt248Rev -> GL_FLOAT_32_UNSIGNED_INT_24_8_REV
  GLDouble -> GL_DOUBLE

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

data Texture1DAttrib = Texture1DAttrib
  { _texture1DAttribBufferFormat :: SizedFormat
  , _texture1DAttribMipmapLevel    :: Int
  , _texture1DAttribBufferWidth    :: Int
  } deriving (Eq, Ord, Show)

data Texture2DAttrib = Texture2DAttrib
  { _texture2DAttribBufferFormat :: SizedFormat
  , _texture2DAttribMipmapLevel    :: Int
  , _texture2DAttribBufferWidth    :: Int
  , _texture2DAttribBufferHeight   :: Int
  } deriving (Eq, Ord, Show)

data Texture3DAttrib = Texture3DAttrib
  { _texture3DAttribBufferFormat :: SizedFormat
  , _texture3DAttribMipmapLevel    :: Int
  , _texture3DAttribBufferWidth    :: Int
  , _texture3DAttribBufferHeight   :: Int
  , _texture3DAttribBufferDepth    :: Int
  } deriving (Eq, Ord, Show)

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

marshallSizedFormat :: SizedFormat -> GLenum
marshallSizedFormat = \case
  SizedR8       -> GL_R8
  SizedR16      -> GL_R16
  SizedR16F     -> GL_R16F
  SizedR32F     -> GL_R32F
  SizedR8I      -> GL_R8I
  SizedR16I     -> GL_R16I
  SizedR32I     -> GL_R32I
  SizedR8UI     -> GL_R8UI
  SizedR16UI    -> GL_R16UI
  SizedR32UI    -> GL_R32UI
  SizedRG8      -> GL_RG8
  SizedRG16     -> GL_RG16
  SizedRG16F    -> GL_RG16F
  SizedRG32F    -> GL_RG32F
  SizedRG8I     -> GL_RG8I
  SizedRG16I    -> GL_RG16I
  SizedRG32I    -> GL_RG32I
  SizedRG8UI    -> GL_RG8UI
  SizedRG16UI   -> GL_RG16UI
  SizedRG32UI   -> GL_RG32UI
  SizedRGB8     -> GL_RGB8
  SizedRGB8UI   -> GL_RGB8UI
  SizedRGB32F   -> GL_RGB32F
  SizedRGB32I   -> GL_RGB32I
  SizedRGB32UI  -> GL_RGB32UI
  SizedRGBA8    -> GL_RGBA8
  SizedRGBA16   -> GL_RGBA16
  SizedRGBA16F  -> GL_RGBA16F
  SizedRGBA32F  -> GL_RGBA32F
  SizedRGBA8I   -> GL_RGBA8I
  SizedRGBA16I  -> GL_RGBA16I
  SizedRGBA32I  -> GL_RGBA32I
  SizedRGBA8UI  -> GL_RGBA8UI
  SizedRGBA16UI -> GL_RGBA16UI
  SizedRGBA32UI -> GL_RGBA32UI

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

newtype TextureObject = TextureObject { _getTextureObjectGLuint :: GLuint } deriving (Eq, Ord, Show)

class TextureTarget t where
  type TextureConfig t
  marshallTextureTarget :: t -> GLenum
  createTexture  :: MonadIO m => t -> TextureConfig t -> m TextureObject

instance TextureTarget TextureTarget1D where
  type TextureConfig TextureTarget1D = Texture1DAttrib
  marshallTextureTarget _ = GL_TEXTURE_1D
  createTexture t Texture1DAttrib {..} = liftIO . allocaArray 1 $ \buff -> do
    glCreateTextures targ 1 buff
    tobj <- unsafeHead <$> peekArray 1 buff
    glTextureStorage1D tobj (fromIntegral _texture1DAttribMipmapLevel) (marshallSizedFormat _texture1DAttribBufferFormat) (fromIntegral _texture1DAttribBufferWidth)
    return $ TextureObject tobj
    where
      targ = marshallTextureTarget t

instance TextureTarget TextureTarget2D where
  type TextureConfig TextureTarget2D = Texture2DAttrib
  marshallTextureTarget = \case
    Texture2D        -> GL_TEXTURE_2D
    TextureRectangle -> GL_TEXTURE_RECTANGLE
    TextureCubeMap   -> GL_TEXTURE_CUBE_MAP
    Texture1DArray   -> GL_TEXTURE_1D_ARRAY
  createTexture t Texture2DAttrib {..} = liftIO . allocaArray 1 $ \buff -> do
    glCreateTextures targ 1 buff
    tobj <- unsafeHead <$> peekArray 1 buff
    glTextureStorage2D tobj (fromIntegral _texture2DAttribMipmapLevel) (marshallSizedFormat _texture2DAttribBufferFormat) (fromIntegral _texture2DAttribBufferWidth) (fromIntegral _texture2DAttribBufferHeight)
    return $ TextureObject tobj
    where
      targ = marshallTextureTarget t

instance TextureTarget TextureTarget3D where
  type TextureConfig TextureTarget3D = Texture3DAttrib
  marshallTextureTarget = \case
    Texture3D           -> GL_TEXTURE_3D
    Texture2DArray      -> GL_TEXTURE_2D_ARRAY
    TextureCubeMapArray -> GL_TEXTURE_CUBE_MAP_ARRAY
  createTexture t Texture3DAttrib {..} = liftIO . allocaArray 1 $ \buff -> do
    glCreateTextures targ 1 buff
    tobj <- unsafeHead <$> peekArray 1 buff
    glTextureStorage3D tobj (fromIntegral _texture3DAttribMipmapLevel) (marshallSizedFormat _texture3DAttribBufferFormat) (fromIntegral _texture3DAttribBufferWidth) (fromIntegral _texture3DAttribBufferHeight) (fromIntegral _texture3DAttribBufferDepth)
    return $ TextureObject tobj
    where
      targ = marshallTextureTarget t
