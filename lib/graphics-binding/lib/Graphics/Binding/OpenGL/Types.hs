{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}

module Graphics.Binding.OpenGL.Types where

import Graphics.GL.Types
import Graphics.GL.Core45
import Foreign.Storable
import Control.Lens
import Control.Monad.IO.Class
import Data.ByteString hiding (head)
import Linear
import Foreign.Ptr
import qualified Data.Vector.Storable as VS
import Graphics.Binding.OpenGL.Utils
import Foreign.Marshal.Array
import Data.Bits
import ForeignResource
import Data.Typeable

-- * Bit helpers

newtype BitAnd a = BitAnd { getBitAnd :: a } deriving (Eq, Ord, Show)
newtype BitOr a = BitOr { getBitOr :: a } deriving (Eq, Ord, Show)

instance (Bits a) => Monoid (BitAnd a) where
  mempty = BitAnd zeroBits
  mappend x y = BitAnd $ getBitAnd x .&. getBitAnd y

instance (Bits a) => Monoid (BitOr a) where
  mempty = BitOr zeroBits
  mappend x y = BitOr $ getBitOr x .|. getBitOr y

-- * Boolean marshaling
marshalGLboolean :: Bool -> GLboolean
marshalGLboolean x = if x then GL_TRUE else GL_FALSE

unmarshalGLboolean :: (Eq a, Num a) => a -> Bool
unmarshalGLboolean = (/= GL_FALSE)

-- * Buffer Objects
newtype BufferObject = BufferObject
  { getBufferObjectGLuint :: GLuint
  } deriving (Eq, Ord, Show, Storable)

instance ForeignName BufferObject () where
  genNames_ n _ = fmap (fmap BufferObject) . liftIO . allocaArray n $
    \ptr -> glCreateBuffers (fromIntegral n) ptr >> peekArray n ptr

  isName_ (BufferObject n) = unmarshalGLboolean <$> glIsBuffer n

  deleteNames_ ns = liftIO . withArrayLen ns $ \len ptr -> glDeleteBuffers (fromIntegral len) (castPtr ptr)

newtype BufferObjectSize = BufferObjectSize
  { bufferObjectSizeInternal :: GLsizeiptr
  } deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype BufferObjectOffset = BufferObjectOffset
  { bufferObjectOffsetInternal :: GLintptr
  } deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype BufferObjectRelOffset = BufferObjectRelOffset
  { bufferObjectRelOffsetInternal :: GLuint
  } deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype BufferObjectStride = BufferObjectStride
  { bufferObjectStrideInternal :: GLsizei
  } deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype BufferObjectComponentSize = BufferObjectComponentSize
  { bufferObjectComponentSize :: GLint
  } deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype BufferObjectIndex = BufferObjectIndex
  { bufferObjectIndexInternal :: GLuint
  } deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

data IntegerHandling
  = Normalized
  | NotNormalized
  deriving (Eq, Ord, Show)

data BufferObjectMapType
  = MapNone
  | MapRead
  | MapWrite
  | MapReadWrite
  deriving (Eq, Ord, Show)

data BufferObjectAttribFlags = BufferObjectAttribFlags
  { _bufferObjectAttribFlagsMapType       :: BufferObjectMapType
  , _bufferObjectAttribFlagsMapPersistent :: Bool
  , _bufferObjectAttribFlagsMapCoherent   :: Bool
  , _bufferObjectAttribFlagsMapDynamic    :: Bool
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

marshalBufferObjectAttribFlags :: BufferObjectAttribFlags -> GLbitfield
marshalBufferObjectAttribFlags BufferObjectAttribFlags {..}
  = getBitOr . mconcat . fmap BitOr $
    [bdyna, brewr, bpers, bcohe, bclie]
  where
    brewr = case _bufferObjectAttribFlagsMapType of
      MapNone -> 0
      MapRead -> GL_MAP_READ_BIT
      MapWrite -> GL_MAP_WRITE_BIT
      MapReadWrite -> GL_MAP_READ_BIT .|. GL_MAP_WRITE_BIT
    bpers = if _bufferObjectAttribFlagsMapPersistent then GL_MAP_PERSISTENT_BIT else 0
    bcohe = if _bufferObjectAttribFlagsMapCoherent then GL_MAP_COHERENT_BIT else 0
    bdyna = if _bufferObjectAttribFlagsMapDynamic then GL_DYNAMIC_STORAGE_BIT else 0
    bclie = if _bufferObjectAttribFlagsClientStorage then GL_CLIENT_STORAGE_BIT else 0

data BufferObjectMapFlags = BufferObjectMapFlags
  { _bufferObjectMapFlagsMapType             :: BufferObjectMapType
  , _bufferObjectMapFlagsMapPersistent       :: Bool
  , _bufferObjectMapFlagsMapCoherent         :: Bool
  , _bufferObjectMapFlagsMapInvalidateRange  :: Bool
  , _bufferObjectMapFlagsMapInvalidateBuffer :: Bool
  , _bufferObjectMapFlagsMapFlushExplicit    :: Bool
  , _bufferObjectMapFlagsMapUnsynchronized   :: Bool
  } deriving (Eq, Ord, Show)

defaultBufferObjectMapFlags :: BufferObjectMapFlags
defaultBufferObjectMapFlags = BufferObjectMapFlags
  { _bufferObjectMapFlagsMapType             = MapNone
  , _bufferObjectMapFlagsMapPersistent       = False
  , _bufferObjectMapFlagsMapCoherent         = False
  , _bufferObjectMapFlagsMapInvalidateRange  = False
  , _bufferObjectMapFlagsMapInvalidateBuffer = False
  , _bufferObjectMapFlagsMapFlushExplicit    = False
  , _bufferObjectMapFlagsMapUnsynchronized   = False
  }

marshalBufferObjectMapFlags :: BufferObjectMapFlags -> GLbitfield
marshalBufferObjectMapFlags BufferObjectMapFlags {..}
  = getBitOr . mconcat . fmap BitOr $
  [brewr, bpers, bcohe, bira, bibu, bfle, bmun]
  where
    brewr = case _bufferObjectMapFlagsMapType of
      MapNone -> 0
      MapRead -> GL_MAP_READ_BIT
      MapWrite -> GL_MAP_WRITE_BIT
      MapReadWrite -> GL_MAP_READ_BIT .|. GL_MAP_WRITE_BIT
    bpers = if _bufferObjectMapFlagsMapPersistent then GL_MAP_PERSISTENT_BIT else 0
    bcohe = if _bufferObjectMapFlagsMapCoherent then GL_MAP_COHERENT_BIT else 0
    bira  = if _bufferObjectMapFlagsMapInvalidateRange then GL_MAP_INVALIDATE_RANGE_BIT else 0
    bibu  = if _bufferObjectMapFlagsMapInvalidateBuffer then GL_MAP_INVALIDATE_BUFFER_BIT else 0
    bfle  = if _bufferObjectMapFlagsMapFlushExplicit then GL_MAP_FLUSH_EXPLICIT_BIT else 0
    bmun  = if _bufferObjectMapFlagsMapUnsynchronized then GL_MAP_UNSYNCHRONIZED_BIT else 0

newtype SizedBufferObject = SizedBufferObject { getSizedBufferObjectGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)

instance ForeignName SizedBufferObject (BufferObjectSize, BufferObjectAttribFlags) where
  genName_ (size, attrib@BufferObjectAttribFlags {..}) = do
    (BufferObject n) <- genName'
    glNamedBufferStorage n (bufferObjectSizeInternal size) nullPtr bitF
    return (SizedBufferObject n)
      where
        bitF = marshalBufferObjectAttribFlags attrib

  isName_ (SizedBufferObject n) = unmarshalGLboolean <$> glIsBuffer n

  deleteNames_ ns = liftIO . withArrayLen ns $ \len ptr -> glDeleteBuffers (fromIntegral len) (castPtr ptr)

data PersistentBuffer a = PersistentBuffer
  { _getPersistentBufferPtr    :: Ptr a
  , _getPersistentBufferName   :: SizedBufferObject
  , _getPersistentBufferSync   :: GLsync
  } deriving (Eq, Ord, Show)

persistentBufferFlagRaw :: GLenum
persistentBufferFlagRaw = GL_MAP_COHERENT_BIT .|. GL_MAP_PERSISTENT_BIT .|. GL_MAP_WRITE_BIT

persistentBufferMapFlag :: BufferObjectMapFlags
persistentBufferMapFlag = BufferObjectMapFlags MapWrite True True False False False False

persistentBufferObjectFlag :: BufferObjectAttribFlags
persistentBufferObjectFlag = BufferObjectAttribFlags MapWrite True True False False

-- * Primitive uniform operations. Note that everything is transposed by default.

newtype UniformLocation = UniformLocation
  { uniformLocationInternal :: GLint
  } deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

class Storable a => PrimUniform a where
  primMarshal_ :: MonadIO m => Program -> UniformLocation -> a -> m ()
  primMarshalArray_ :: MonadIO m => Program -> UniformLocation -> VS.Vector a -> m ()

primMarshal :: (PrimUniform a, MonadIO m) => UniformLocation -> Program -> a -> m ()
primMarshal = flip primMarshal_

primMarshalArray :: (PrimUniform a, MonadIO m) => UniformLocation -> Program -> VS.Vector a -> m ()
primMarshalArray = flip primMarshalArray_

instance PrimUniform GLint where
  primMarshal_ (Program n) (UniformLocation m) = glProgramUniform1i n m
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ glProgramUniform1iv n m

instance PrimUniform GLuint where
  primMarshal_ (Program n) (UniformLocation m) = glProgramUniform1ui n m
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ glProgramUniform1uiv n m

instance PrimUniform GLfloat where
  primMarshal_ (Program n) (UniformLocation m) = glProgramUniform1f n m
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ glProgramUniform1fv n m

instance PrimUniform GLdouble where
  primMarshal_ (Program n) (UniformLocation m) = glProgramUniform1d n m
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ glProgramUniform1dv n m

instance PrimUniform (V1 GLint) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform1iv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform1iv n m len (castPtr ptr)

instance PrimUniform (V1 GLuint) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform1uiv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform1uiv n m len (castPtr ptr)

instance PrimUniform (V1 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform1fv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform1fv n m len (castPtr ptr)

instance PrimUniform (V1 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform1dv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform1dv n m len (castPtr ptr)

instance PrimUniform (V2 GLint) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform2iv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform2iv n m len (castPtr ptr)

instance PrimUniform (V2 GLuint) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform2uiv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform2uiv n m len (castPtr ptr)

instance PrimUniform (V2 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform2fv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform2fv n m len (castPtr ptr)

instance PrimUniform (V2 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform2dv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform2dv n m len (castPtr ptr)

instance PrimUniform (V3 GLint) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform3iv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform3iv n m len (castPtr ptr)

instance PrimUniform (V3 GLuint) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform3uiv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform3uiv n m len (castPtr ptr)

instance PrimUniform (V3 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform3fv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform3fv n m len (castPtr ptr)

instance PrimUniform (V3 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform3dv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform3dv n m len (castPtr ptr)

instance PrimUniform (V4 GLint) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform4iv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform4iv n m len (castPtr ptr)

instance PrimUniform (V4 GLuint) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform4uiv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform4uiv n m len (castPtr ptr)

instance PrimUniform (V4 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform4fv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform4fv n m len (castPtr ptr)

instance PrimUniform (V4 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform4dv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform4dv n m len (castPtr ptr)

-- Column major!

instance PrimUniform (M22 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix2fv n m 1 GL_TRUE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix2fv n m len GL_TRUE (castPtr ptr)

instance PrimUniform (M23 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix2x3fv n m 1 GL_TRUE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix2x3fv n m len GL_TRUE (castPtr ptr)

instance PrimUniform (M24 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix2x4fv n m 1 GL_TRUE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix2x4fv n m len GL_TRUE (castPtr ptr)

instance PrimUniform (M32 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix3x2fv n m 1 GL_TRUE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix3x2fv n m len GL_TRUE (castPtr ptr)

instance PrimUniform (M33 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix3fv n m 1 GL_TRUE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix3fv n m len GL_TRUE (castPtr ptr)

instance PrimUniform (M34 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix3x4fv n m 1 GL_TRUE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix3x4fv n m len GL_TRUE (castPtr ptr)

instance PrimUniform (M42 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix4x2fv n m 1 GL_TRUE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix4x2fv n m len GL_TRUE (castPtr ptr)

instance PrimUniform (M43 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix4x3fv n m 1 GL_TRUE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix4x3fv n m len GL_TRUE (castPtr ptr)

instance PrimUniform (M44 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix4fv n m 1 GL_TRUE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix4fv n m len GL_TRUE (castPtr ptr)

instance PrimUniform (M22 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix2dv n m 1 GL_TRUE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix2dv n m len GL_TRUE (castPtr ptr)

instance PrimUniform (M23 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix2x3dv n m 1 GL_TRUE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix2x3dv n m len GL_TRUE (castPtr ptr)

instance PrimUniform (M24 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix2x4dv n m 1 GL_TRUE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix2x4dv n m len GL_TRUE (castPtr ptr)

instance PrimUniform (M32 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix3x2dv n m 1 GL_TRUE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix3x2dv n m len GL_TRUE (castPtr ptr)

instance PrimUniform (M33 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix3dv n m 1 GL_TRUE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix3dv n m len GL_TRUE (castPtr ptr)

instance PrimUniform (M34 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix3x4dv n m 1 GL_TRUE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix3x4dv n m len GL_TRUE (castPtr ptr)

instance PrimUniform (M42 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix4x2dv n m 1 GL_TRUE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix4x2dv n m len GL_TRUE (castPtr ptr)

instance PrimUniform (M43 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix4x3dv n m 1 GL_TRUE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix4x3dv n m len GL_TRUE (castPtr ptr)

instance PrimUniform (M44 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix4dv n m 1 GL_TRUE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix4dv n m len GL_TRUE (castPtr ptr)

-- * Rendering
data ClearBuffer = ClearBuffer
  { _clearBufferColor   :: Bool
  , _clearBufferStencil :: Bool
  , _clearBufferDepth   :: Bool
  } deriving (Eq, Ord, Show)

defaultClearBuffer :: ClearBuffer
defaultClearBuffer = ClearBuffer False False False

marshalClearBuffer :: ClearBuffer -> GLbitfield
marshalClearBuffer ClearBuffer {..}
  = cbit .|. sbit .|. dbit
  where
    cbit = GL_COLOR_BUFFER_BIT
    sbit = GL_STENCIL_BUFFER_BIT
    dbit = GL_DEPTH_BUFFER_BIT

data PrimitiveMode
  = Points
  | LineStrip
  | LineLoop
  | Lines
  | LineStripAdjacency
  | LinesAdjacency
  | TriangleStrip
  | TriangleFan
  | Triangles
  | TriangleStripAdjacency
  | TrianglesAdjacency
  | Patches
  deriving (Eq, Ord, Show)

marshalPrimitiveMode :: PrimitiveMode -> GLenum
marshalPrimitiveMode = \case
  Points                 -> GL_POINTS
  LineStrip              -> GL_LINE_STRIP
  LineLoop               -> GL_LINE_LOOP
  Lines                  -> GL_LINES
  LineStripAdjacency     -> GL_LINE_STRIP_ADJACENCY
  LinesAdjacency         -> GL_LINES_ADJACENCY
  TriangleStrip          -> GL_TRIANGLE_STRIP
  TriangleFan            -> GL_TRIANGLE_FAN
  Triangles              -> GL_TRIANGLES
  TriangleStripAdjacency -> GL_TRIANGLE_STRIP_ADJACENCY
  TrianglesAdjacency     -> GL_TRIANGLES_ADJACENCY
  Patches                -> GL_PATCHES

data IndexType
  = UnsignedByte
  | UnsignedShort
  | UnsignedInt
  deriving (Eq, Ord, Show)

marshalIndexType :: IndexType -> GLenum
marshalIndexType = \case
  UnsignedByte  -> GL_UNSIGNED_BYTE
  UnsignedShort -> GL_UNSIGNED_SHORT
  UnsignedInt   -> GL_UNSIGNED_INT

-- * Shader programs
newtype Program = Program { getProgramGLuint :: GLuint } deriving (Eq, Ord, Show)

instance ForeignName Program () where
  genName_ _ = Program <$> glCreateProgram
  isName_ (Program n) = unmarshalGLboolean <$> glIsProgram n
  deleteName_ (Program n) = glDeleteProgram n

-- * Shaders
newtype VertexShader = VertexShader { _vertexShaderGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)

instance ForeignName VertexShader () where
  genName_ _ = VertexShader <$> glCreateShader GL_VERTEX_SHADER
  isName_ (VertexShader n) = unmarshalGLboolean <$> glIsShader n
  deleteName_ (VertexShader n) = glDeleteShader n

newtype FragmentShader = FragmentShader { _fragmentShaderGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)

instance ForeignName FragmentShader () where
  genName_ _ = FragmentShader <$> glCreateShader GL_FRAGMENT_SHADER
  isName_ (FragmentShader n) = unmarshalGLboolean <$> glIsShader n
  deleteName_ (FragmentShader n) = glDeleteShader n

newtype TessControlShader = TessControlShader { _tessControlShaderGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)

instance ForeignName TessControlShader () where
  genName_ _ = TessControlShader <$> glCreateShader GL_TESS_CONTROL_SHADER
  isName_ (TessControlShader n) = unmarshalGLboolean <$> glIsShader n
  deleteName_ (TessControlShader n) = glDeleteShader n

newtype TessEvalShader = TessEvalShader { _tessEvalShaderGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)

instance ForeignName TessEvalShader () where
  genName_ _ = TessEvalShader <$> glCreateShader GL_TESS_EVALUATION_SHADER
  isName_ (TessEvalShader n) = unmarshalGLboolean <$> glIsShader n
  deleteName_ (TessEvalShader n) = glDeleteShader n

newtype ComputeShader = ComputeShader { _computeShaderGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)

instance ForeignName ComputeShader () where
  genName_ _ = ComputeShader <$> glCreateShader GL_COMPUTE_SHADER
  isName_ (ComputeShader n) = unmarshalGLboolean <$> glIsShader n
  deleteName_ (ComputeShader n) = glDeleteShader n

class ForeignName t () => Shader t where
  marshalShaderType :: t -> GLuint
  marshalShaderObject :: t -> GLuint

instance Shader VertexShader where
  marshalShaderType _ = GL_VERTEX_SHADER
  marshalShaderObject = _vertexShaderGLuint

instance Shader TessEvalShader where
  marshalShaderType _ = GL_TESS_EVALUATION_SHADER
  marshalShaderObject = _tessEvalShaderGLuint

instance Shader TessControlShader where
  marshalShaderType _ = GL_TESS_CONTROL_SHADER
  marshalShaderObject = _tessControlShaderGLuint

instance Shader FragmentShader where
  marshalShaderType _ = GL_FRAGMENT_SHADER
  marshalShaderObject = _fragmentShaderGLuint

instance Shader ComputeShader where
  marshalShaderType _ = GL_COMPUTE_SHADER
  marshalShaderObject = _computeShaderGLuint

-- * GL Storable types
class GLSized a where
  gSize_ :: Proxy a -> Int

gSize :: (Integral n, GLSized a) => Proxy a -> n
gSize = fromIntegral . gSize_

class GLSized a => GLWritable a where
  gPoke_ :: Ptr a -> a -> IO ()

gPoke :: (GLWritable a, MonadIO m) => Ptr a -> a -> m ()
gPoke ptr = liftIO . gPoke_ ptr

class GLSized a => GLReadable a where
  gPeek_ :: Ptr a -> IO a

gPeek :: (GLReadable a, MonadIO m) => Ptr a -> m a
gPeek = liftIO . gPeek_

-- * Textures

newtype TextureUnit = TextureUnit
  { getTextureUnitGLuint :: GLuint
  } deriving (Eq, Ord, Show, Num)

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

instance TextureTarget t => ForeignName (TextureObject t) t where
  genNames_ n t = fmap (fmap TextureObject) . liftIO . allocaArray n $
    \ptr -> glCreateTextures targ (fromIntegral n) ptr >> peekArray n ptr
    where
      targ = marshalTextureTarget t

  isName_ (TextureObject n) = unmarshalGLboolean <$> glIsTexture n

  deleteNames_ ns = liftIO . withArrayLen ns $ \len ptr -> glDeleteTextures (fromIntegral len) (castPtr ptr)

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

marshalPixelFormat :: PixelFormat -> GLenum
marshalPixelFormat = \case
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
  marshalTextureTarget :: t -> GLenum
  createTexture  :: MonadIO m => t -> TextureConfig t -> m (TextureObject t)
  textureSubMap :: MonadIO m => TextureObject t -> Int -> PixelConfig t -> Ptr () -> m ()

genTextureNames :: (MonadIO m, TextureTarget t) => Int -> t -> m [TextureObject t]
genTextureNames n t = fmap TextureObject <$> (liftIO . allocaArray n $ \ptr -> glCreateTextures targ (fromIntegral n) ptr >> peekArray n ptr)
  where
    targ = marshalTextureTarget t

genTextureName :: (MonadIO m, TextureTarget t) => t -> m (TextureObject t)
genTextureName t = head <$> genTextureNames 1 t

instance TextureTarget TextureTarget1D where
  type TextureConfig TextureTarget1D = Texture1DAttrib

  type PixelConfig TextureTarget1D = Pixel1DAttrib

  marshalTextureTarget _ = GL_TEXTURE_1D

  createTexture t Texture1DAttrib {..} = do
    tobj@(TextureObject n) <- genTextureName t
    glTextureStorage1D n (fromIntegral _texture1DAttribMipmapLevel) (marshalSizedFormat _texture1DAttribBufferFormat) (fromIntegral _texture1DAttribBufferWidth)
    return tobj

  textureSubMap (TextureObject tobj) level Pixel1DAttrib {..} = glTextureSubImage1D tobj lev xo width pixForm datType
    where
      xo = maybe 0 fromIntegral _pixel1DAttribPixelXOffset
      lev = fromIntegral level
      pixForm = marshalPixelFormat _pixel1DAttribPixelFormat
      datType = marshalGLDataType _pixel1DAttribPixelType
      width  = fromIntegral _pixel1DAttribPixelWidth

instance TextureTarget TextureTarget2D where
  type TextureConfig TextureTarget2D = Texture2DAttrib

  type PixelConfig TextureTarget2D = Pixel2DAttrib

  marshalTextureTarget = \case
    Texture2D        -> GL_TEXTURE_2D
    TextureRectangle -> GL_TEXTURE_RECTANGLE
    TextureCubeMap   -> GL_TEXTURE_CUBE_MAP
    Texture1DArray   -> GL_TEXTURE_1D_ARRAY

  createTexture t Texture2DAttrib {..} = do
    tobj@(TextureObject n) <- genTextureName t
    glTextureStorage2D n (fromIntegral _texture2DAttribMipmapLevel) (marshalSizedFormat _texture2DAttribBufferFormat) (fromIntegral _texture2DAttribBufferWidth) (fromIntegral _texture2DAttribBufferHeight)
    return tobj

  textureSubMap (TextureObject tobj) level Pixel2DAttrib {..} =  glTextureSubImage2D tobj lev xo yo width height pixForm datType
    where
      xo = maybe 0 fromIntegral _pixel2DAttribPixelXOffset
      yo = maybe 0 fromIntegral _pixel2DAttribPixelYOffset
      lev = fromIntegral level
      pixForm = marshalPixelFormat _pixel2DAttribPixelFormat
      datType = marshalGLDataType _pixel2DAttribPixelType
      width  = fromIntegral _pixel2DAttribPixelWidth
      height = fromIntegral _pixel2DAttribPixelHeight

instance TextureTarget TextureTarget3D where
  type TextureConfig TextureTarget3D = Texture3DAttrib

  type PixelConfig TextureTarget3D = Pixel3DAttrib

  marshalTextureTarget = \case
    Texture3D           -> GL_TEXTURE_3D
    Texture2DArray      -> GL_TEXTURE_2D_ARRAY
    TextureCubeMapArray -> GL_TEXTURE_CUBE_MAP_ARRAY

  createTexture t Texture3DAttrib {..} = do
    tobj@(TextureObject n) <- genTextureName t
    glTextureStorage3D n (fromIntegral _texture3DAttribMipmapLevel) (marshalSizedFormat _texture3DAttribBufferFormat) (fromIntegral _texture3DAttribBufferWidth) (fromIntegral _texture3DAttribBufferHeight) (fromIntegral _texture3DAttribBufferDepth)
    return tobj

  textureSubMap (TextureObject tobj) level Pixel3DAttrib {..} = glTextureSubImage3D tobj lev xo yo zo width height depth pixForm datType
    where
      xo = maybe 0 fromIntegral _pixel3DAttribPixelXOffset
      yo = maybe 0 fromIntegral _pixel3DAttribPixelYOffset
      zo = maybe 0 fromIntegral _pixel3DAttribPixelZOffset
      lev = fromIntegral level
      pixForm = marshalPixelFormat _pixel3DAttribPixelFormat
      datType = marshalGLDataType _pixel3DAttribPixelType
      width  = fromIntegral _pixel3DAttribPixelWidth
      height = fromIntegral _pixel3DAttribPixelHeight
      depth  = fromIntegral _pixel3DAttribPixelDepth

marshalTextureParameter :: TextureParameter -> GLenum
marshalTextureParameter = \case
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

class TextureSampler a where
  type TextureSamplerTarget a
  type TextureSamplerType a :: SamplerType
  texture :: (MonadIO m, TextureTarget t, t ~ TextureSamplerTarget a) => TextureObject t -> a -> m ()

-- * Vertex array objects

newtype VertexArrayObject = VertexArrayObject
  { getVertexArrayObjectGLuint :: GLuint
  } deriving (Eq, Ord, Show, Storable)

instance ForeignName VertexArrayObject () where
  genNames_ n _ = fmap (fmap VertexArrayObject) . liftIO . allocaArray n $
    \ptr -> glCreateVertexArrays (fromIntegral n) ptr >> peekArray n ptr
  isName_ (VertexArrayObject n) = unmarshalGLboolean <$> glIsBuffer n
  deleteNames_ ns = liftIO . withArrayLen ns $ \len ptr -> glDeleteVertexArrays (fromIntegral len) (castPtr ptr)

newtype AttribLocation = AttribLocation { getAttribLocationGLuint :: GLuint } deriving (Eq, Ord, Show, Num)

-- * Window
data Face = Front | Back | FrontBack deriving (Eq, Ord, Show)
newtype Color4 = Color4 (V4 GLfloat)

data DepthFunc
  = DepthNever
  | DepthLess
  | DepthEqual
  | DepthLEqual
  | DepthGreater
  | DepthNotEqual
  | DepthGEqual
  | DepthAlways
  deriving (Eq, Ord, Show)

data DebugSource
  = DebugSourceAPI
  | DebugSourceWindowSystem
  | DebugSourceShaderCompiler
  | DebugSourceThirdParty
  | DebugSourceApplication
  | DebugSourceOther
  deriving (Eq, Ord, Show)

data DebugType
  = DebugTypeError
  | DebugTypeDeprecatedBehavior
  | DebugTypeUndefinedBehavior
  | DebugTypePortability
  | DebugTypePerformance
  | DebugTypeMarker
  | DebugTypePushGroup
  | DebugTypePopGroup
  | DebugTypeOther
  deriving (Eq, Ord, Show)

data DebugSeverity
  = DebugSeverityHigh
  | DebugSeverityMedium
  | DebugSeverityLow
  | DebugSeverityNotification
  deriving (Eq, Ord, Show)

newtype DebugID = DebugID GLuint deriving (Eq, Ord, Show)

type DebugCallbackFun = DebugSource -> DebugType -> DebugID -> DebugSeverity -> ByteString -> IO ()

unmarshallDebugSource :: GLenum -> DebugSource
unmarshallDebugSource = \case
  GL_DEBUG_SOURCE_API             -> DebugSourceAPI
  GL_DEBUG_SOURCE_WINDOW_SYSTEM   -> DebugSourceWindowSystem
  GL_DEBUG_SOURCE_SHADER_COMPILER -> DebugSourceShaderCompiler
  GL_DEBUG_SOURCE_THIRD_PARTY     -> DebugSourceThirdParty
  GL_DEBUG_SOURCE_APPLICATION     -> DebugSourceApplication
  GL_DEBUG_SOURCE_OTHER           -> DebugSourceOther
  _                            -> error "Unrecognized debug source.\n"

unmarshallDebugType   :: GLenum -> DebugType
unmarshallDebugType = \case
  GL_DEBUG_TYPE_ERROR               -> DebugTypeError
  GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR -> DebugTypeDeprecatedBehavior
  GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR  -> DebugTypeUndefinedBehavior
  GL_DEBUG_TYPE_PORTABILITY         -> DebugTypePortability
  GL_DEBUG_TYPE_PERFORMANCE         -> DebugTypePerformance
  GL_DEBUG_TYPE_MARKER              -> DebugTypeMarker
  GL_DEBUG_TYPE_PUSH_GROUP          -> DebugTypePushGroup
  GL_DEBUG_TYPE_POP_GROUP           -> DebugTypePopGroup
  GL_DEBUG_TYPE_OTHER               -> DebugTypeOther
  _                              -> error "Unrecognized debug type.\n"

unmarshallDebugSeverity :: GLenum -> DebugSeverity
unmarshallDebugSeverity = \case
  GL_DEBUG_SEVERITY_HIGH         -> DebugSeverityHigh
  GL_DEBUG_SEVERITY_MEDIUM       -> DebugSeverityMedium
  GL_DEBUG_SEVERITY_LOW          -> DebugSeverityLow
  GL_DEBUG_SEVERITY_NOTIFICATION -> DebugSeverityNotification
  _                           -> error "Unrecognized debug severity.\n"

-- * GL Data Types

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
  deriving (Eq, Ord, Show)

marshalGLDataType :: GLDataType -> GLenum
marshalGLDataType = \case
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
  deriving (Eq, Ord, Show)

marshalSizedFormat :: SizedFormat -> GLenum
marshalSizedFormat = \case
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

marshalBufferAccess :: BufferAccess -> GLenum
marshalBufferAccess = \case
   ReadOnly -> GL_READ_ONLY
   WriteOnly -> GL_WRITE_ONLY
   ReadWrite -> GL_READ_WRITE

data Capability = Enabled | Disabled deriving (Eq, Ord, Show)

data BufferBindPoint
  = BufferAtomicCounter
  | BufferTransformFeedback
  | BufferUniform
  | BufferShaderStorage
  deriving (Eq, Ord, Show)

marshalBufferBindPoint :: BufferBindPoint -> GLenum
marshalBufferBindPoint = \case
  BufferAtomicCounter     -> GL_ATOMIC_COUNTER_BUFFER
  BufferTransformFeedback -> GL_TRANSFORM_FEEDBACK_BUFFER
  BufferUniform           -> GL_UNIFORM_BUFFER
  BufferShaderStorage     -> GL_SHADER_STORAGE_BUFFER

data SamplerType
  = SamplerFloat
  | SamplerInt
  | SamplerUnsignedInt
  deriving (Eq, Ord, Show)

marshalSamplerType :: SamplerType -> GLenum
marshalSamplerType = \case
  SamplerUnsignedInt -> GL_UNSIGNED_INT
  SamplerInt         -> GL_INT
  SamplerFloat       -> GL_FLOAT

-- * Uniforms
type DefaultBlock = Program

newtype BlockBindingPoint = BlockBindingPoint { _getBlockBindingPointGLuint :: GLuint}

class Uniform a where
  type UniformContents a
  type UniformLocationType a
  uniform :: MonadIO m => UniformLocationType a -> a -> UniformContents a -> m ()

class UniformBlock a b where
  bindBlock_ :: a -> b -> IO ()

bindBlock :: (UniformBlock a b, MonadIO m) => a -> b -> m ()
bindBlock a = liftIO . bindBlock_ a

makeFields ''Texture1DAttrib
makeFields ''Texture2DAttrib
makeFields ''Texture3DAttrib
makeFields ''Pixel1DAttrib
makeFields ''Pixel2DAttrib
makeFields ''Pixel3DAttrib
makeFields ''BufferObjectAttribFlags
makeFields ''BufferObjectMapFlags

mconcat <$> mapM makeLenses
  [ ''ClearBuffer
  ]
