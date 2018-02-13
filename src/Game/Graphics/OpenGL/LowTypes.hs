{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Foreign.Ptr
import Game.Graphics.OpenGL.Utils
import qualified Data.Vector.Storable as VS

class Storable a => ObjectName a where
  isObjectName :: MonadIO m => a -> m Bool
  deleteObjectName :: MonadIO m => a -> m ()
  deleteObjectNames :: MonadIO m => VS.Vector a -> m ()

  deleteObjectName = deleteObjectNames . VS.singleton
  deleteObjectNames = VS.mapM_ deleteObjectName

class ObjectName a => GeneratableObjectName a where
  genObjectName :: MonadIO m => m a
  genObjectNames :: MonadIO m => Int -> m (VS.Vector a)

  genObjectName = VS.head <$> genObjectNames 1
  genObjectNames n = VS.replicateM n genObjectName

marshallGLboolean :: Bool -> GLboolean
marshallGLboolean x = if x then GL_TRUE else GL_FALSE

unmarshallGLboolean :: (Eq a, Num a) => a -> Bool
unmarshallGLboolean = (/= GL_FALSE)

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

newtype BufferObject = BufferObject { getBufferObjectGLuint :: GLuint }
  deriving (Eq, Ord, Show, Storable)

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

instance ObjectName BufferObject where
  isObjectName (BufferObject n) = unmarshallGLboolean <$> glIsBuffer n
  deleteObjectNames ns = liftIO . VS.unsafeWith ns $ \ptr -> glDeleteBuffers len (castPtr ptr)
    where
      len = fromIntegral $ VS.length ns

instance GeneratableObjectName BufferObject where
  genObjectNames n = VS.map BufferObject <$> withForeignBufferVec n (glCreateBuffers (fromIntegral n))

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
   | GLDouble
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
  GLDouble -> GL_DOUBLE
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

newtype TextureObject t = TextureObject { _getTextureObjectGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)

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

newtype VertexArrayObject = VertexArrayObject { getVertexArrayObjectGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)

instance ObjectName VertexArrayObject where
  isObjectName (VertexArrayObject n) = unmarshallGLboolean <$> glIsVertexArray n
  deleteObjectNames ns = liftIO . VS.unsafeWith ns $ \ptr -> glDeleteVertexArrays len (castPtr ptr)
    where
      len = fromIntegral $ VS.length ns

instance GeneratableObjectName VertexArrayObject where
  genObjectNames n = VS.map VertexArrayObject <$> withForeignBufferVec n (glCreateVertexArrays(fromIntegral n))

newtype AttribLocation = AttribLocation { getAttribLocationGLuint :: GLuint } deriving (Eq, Ord, Show)

data BufferAccess = ReadOnly | WriteOnly | ReadWrite deriving (Eq, Ord, Show)

marshallBufferAccess :: BufferAccess -> GLenum
marshallBufferAccess = \case
   ReadOnly -> GL_READ_ONLY
   WriteOnly -> GL_WRITE_ONLY
   ReadWrite -> GL_READ_WRITE

data Capability = Enabled | Disabled deriving (Eq, Ord, Show)

newtype TextureUnit = TextureUnit { getTextureUnitGLuint :: GLuint } deriving (Eq, Ord, Show)

newtype VertexShader = VertexShader { _vertexShaderGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)
instance ObjectName VertexShader where
  isObjectName (VertexShader n) = unmarshallGLboolean <$> glIsShader n
  deleteObjectName (VertexShader n) = glDeleteShader n
instance GeneratableObjectName VertexShader where
  genObjectName = VertexShader <$> glCreateShader GL_VERTEX_SHADER

newtype FragmentShader = FragmentShader { _fragmentShaderGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)
instance ObjectName FragmentShader where
  isObjectName (FragmentShader n) = unmarshallGLboolean <$> glIsShader n
  deleteObjectName (FragmentShader n) = glDeleteShader n
instance GeneratableObjectName FragmentShader where
  genObjectName = FragmentShader <$> glCreateShader GL_FRAGMENT_SHADER

newtype TessControlShader = TessControlShader { _tessControlShaderGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)
instance ObjectName TessControlShader where
  isObjectName (TessControlShader n) = unmarshallGLboolean <$> glIsShader n
  deleteObjectName (TessControlShader n) = glDeleteShader n
instance GeneratableObjectName TessControlShader where
  genObjectName = TessControlShader <$> glCreateShader GL_TESS_CONTROL_SHADER

newtype TessEvalShader = TessEvalShader { _tessEvalShaderGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)
instance ObjectName TessEvalShader where
  isObjectName (TessEvalShader n) = unmarshallGLboolean <$> glIsShader n
  deleteObjectName (TessEvalShader n) = glDeleteShader n
instance GeneratableObjectName TessEvalShader where
  genObjectName = TessEvalShader <$> glCreateShader GL_TESS_EVALUATION_SHADER

newtype ComputeShader = ComputeShader { _computeShaderGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)
instance ObjectName ComputeShader where
  isObjectName (ComputeShader n) = unmarshallGLboolean <$> glIsShader n
  deleteObjectName (ComputeShader n) = glDeleteShader n
instance GeneratableObjectName ComputeShader where
  genObjectName = ComputeShader <$> glCreateShader GL_COMPUTE_SHADER

class GeneratableObjectName t => Shader t where
  marshallShaderType :: t -> GLuint
  marshallShaderObject :: t -> GLuint

instance Shader VertexShader where
  marshallShaderType _ = GL_VERTEX_SHADER
  marshallShaderObject = _vertexShaderGLuint
instance Shader TessEvalShader where
  marshallShaderType _ = GL_TESS_EVALUATION_SHADER
  marshallShaderObject = _tessEvalShaderGLuint
instance Shader TessControlShader where
  marshallShaderType _ = GL_TESS_CONTROL_SHADER
  marshallShaderObject = _tessControlShaderGLuint
instance Shader FragmentShader where
  marshallShaderType _ = GL_FRAGMENT_SHADER
  marshallShaderObject = _fragmentShaderGLuint
instance Shader ComputeShader where
  marshallShaderType _ = GL_COMPUTE_SHADER
  marshallShaderObject = _computeShaderGLuint

newtype Program = Program { getProgramGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)
instance ObjectName Program where
  isObjectName (Program n) = unmarshallGLboolean <$> glIsProgram n
  deleteObjectName (Program n) = glDeleteProgram n

instance GeneratableObjectName Program where
  genObjectName = Program <$> glCreateProgram

