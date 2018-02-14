{-# LANGUAGE LambdaCase #-}

module Game.Graphics.OpenGL.DataType where

import Graphics.GL.Types
import Graphics.GL.Core45

data GLDataType
  = GLUnsignedByte
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

data BufferAccess = ReadOnly | WriteOnly | ReadWrite deriving (Eq, Ord, Show)

marshallBufferAccess :: BufferAccess -> GLenum
marshallBufferAccess = \case
   ReadOnly -> GL_READ_ONLY
   WriteOnly -> GL_WRITE_ONLY
   ReadWrite -> GL_READ_WRITE

data Capability = Enabled | Disabled deriving (Eq, Ord, Show)

