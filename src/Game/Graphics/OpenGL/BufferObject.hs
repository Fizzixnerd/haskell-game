{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Graphics.OpenGL.BufferObject where

import Graphics.GL.Core45
import Graphics.GL.Types
import Control.Lens
import Game.Graphics.OpenGL.ObjectName
import Game.Graphics.OpenGL.Boolean
import Game.Graphics.OpenGL.Utils
import qualified Data.Vector.Storable as VS
import Game.Graphics.OpenGL.DataType
import Data.Bits ((.|.))

newtype BufferObject = BufferObject
  { getBufferObjectGLuint :: GLuint
  } deriving (Eq, Ord, Show, Storable)

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

-- Should probably remove. I'm lazy
toBufferObjectOffset :: Integral a => a -> BufferObjectOffset
toBufferObjectOffset = BufferObjectOffset . fromIntegral

toBufferObjectSize :: Integral a => a -> BufferObjectSize
toBufferObjectSize = BufferObjectSize . fromIntegral

toBufferObjectStride :: Integral a => a -> BufferObjectStride
toBufferObjectStride = BufferObjectStride . fromIntegral


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

makeFields ''BufferObjectAttribFlags
makeFields ''BufferObjectMapFlags

instance ObjectName BufferObject where
  isObjectName (BufferObject n) = unmarshallGLboolean <$> glIsBuffer n
  deleteObjectNames ns = liftIO . VS.unsafeWith ns $ \ptr -> glDeleteBuffers len (castPtr ptr)
    where
      len = fromIntegral $ VS.length ns

instance GeneratableObjectName BufferObject where
  genObjectNames n = VS.map BufferObject <$> withForeignBufferVec n (glCreateBuffers (fromIntegral n))

-----------------------------

marshallBufferObjectAttribFlags :: BufferObjectAttribFlags -> GLbitfield
marshallBufferObjectAttribFlags BufferObjectAttribFlags {..}
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

marshallBufferObjectMapFlags :: BufferObjectMapFlags -> GLbitfield
marshallBufferObjectMapFlags BufferObjectMapFlags {..}
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


genBufferObject :: MonadIO m => BufferObjectSize -> BufferObjectAttribFlags -> m BufferObject
genBufferObject size attrib@BufferObjectAttribFlags {..} = do
  bufo@(BufferObject n) <- genObjectName
  glNamedBufferStorage n (bufferObjectSizeInternal size) nullPtr bitF
  return bufo
  where
    bitF = marshallBufferObjectAttribFlags attrib

initBufferObject :: MonadIO m
                 => BufferObjectSize
                 -> BufferObjectAttribFlags
                 -> Ptr ()
                 -> m BufferObject
initBufferObject size attrib@BufferObjectAttribFlags {..} ptr = do
  bufo@(BufferObject n) <- genObjectName
  glNamedBufferStorage n (bufferObjectSizeInternal size) ptr bitF
  return bufo
  where
    bitF = marshallBufferObjectAttribFlags attrib

bufferSubData :: MonadIO m => BufferObject -> BufferObjectSize -> BufferObjectOffset -> Ptr () -> m ()
bufferSubData (BufferObject n) (BufferObjectSize size) (BufferObjectOffset m) = glNamedBufferSubData n m size

mapBuffer :: MonadIO m => BufferObject -> BufferAccess -> m (Maybe (Ptr ()))
mapBuffer t = fmap (maybeNullPtr Nothing Just) . mapBuffer_ t

mapBuffer_ :: MonadIO m => BufferObject -> BufferAccess -> m (Ptr ())
mapBuffer_ (BufferObject n) = glMapNamedBuffer n . marshallBufferAccess

unmapBuffer :: MonadIO m => BufferObject -> m Bool
unmapBuffer (BufferObject n) = fmap unmarshallGLboolean . glUnmapNamedBuffer $ n

mapBufferRange :: MonadIO m => BufferObject -> BufferObjectOffset -> BufferObjectSize -> BufferObjectMapFlags -> m (Ptr ())
mapBufferRange (BufferObject obj) offset leng flags
  = glMapNamedBufferRange obj (fromIntegral offset) (fromIntegral leng) (marshallBufferObjectMapFlags flags)

clearBufferSubData :: MonadIO m => BufferObject -> SizedFormat -> BufferObjectOffset -> BufferObjectSize -> SizedFormat -> GLDataType -> Ptr () -> m ()
clearBufferSubData (BufferObject obj) internalForm offset size form typ
  = glClearNamedBufferSubData obj (marshallSizedFormat internalForm) (fromIntegral offset) (fromIntegral size) (marshallSizedFormat form) (marshallGLDataType typ)

copyBufferSubData :: MonadIO m => BufferObject -> BufferObject -> BufferObjectOffset -> BufferObjectOffset -> BufferObjectSize -> m ()
copyBufferSubData (BufferObject readB) (BufferObject writeB) readOff writeOff size
  = glCopyNamedBufferSubData readB writeB (fromIntegral readOff) (fromIntegral writeOff) (fromIntegral size)
