{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Binding.OpenGL.BufferObject where

import           Control.Lens
import           Data.Bits ((.|.))
import           Graphics.Binding.OpenGL.Boolean
import           Graphics.Binding.OpenGL.DataType
import           Graphics.Binding.OpenGL.ObjectName
import           Graphics.Binding.OpenGL.Synchro
import           Foreign
import           Graphics.Binding.OpenGL.Utils
import           Graphics.GL.Core45
import           Graphics.GL.Types
import qualified Data.Vector.Storable as VS

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
  isObjectName (BufferObject n) = unmarshalGLboolean <$> glIsBuffer n
  deleteObjectNames ns = liftIO . withArrayLen ns $ \len ptr -> glDeleteBuffers (fromIntegral len) (castPtr ptr)

instance GeneratableObjectName BufferObject where
  genObjectNames_ n = fmap BufferObject <$> (liftIO . allocaArray n $ \ptr -> glCreateBuffers (fromIntegral n) ptr >> peekArray n ptr)

-----------------------------

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

genBufferObject :: MonadIO m => BufferObjectSize -> BufferObjectAttribFlags -> m BufferObject
genBufferObject size attrib@BufferObjectAttribFlags {..} = do
  bufo@(BufferObject n) <- genObjectName
  glNamedBufferStorage n (bufferObjectSizeInternal size) nullPtr bitF
  return bufo
  where
    bitF = marshalBufferObjectAttribFlags attrib

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
    bitF = marshalBufferObjectAttribFlags attrib

bufferSubData :: MonadIO m => BufferObject -> BufferObjectSize -> BufferObjectOffset -> Ptr () -> m ()
bufferSubData (BufferObject n) (BufferObjectSize size) (BufferObjectOffset m) = glNamedBufferSubData n m size

mapBuffer :: MonadIO m => BufferObject -> BufferAccess -> m (Maybe (Ptr ()))
mapBuffer t = fmap (maybeNullPtr Nothing Just) . mapBuffer_ t

mapBuffer_ :: MonadIO m => BufferObject -> BufferAccess -> m (Ptr ())
mapBuffer_ (BufferObject n) = glMapNamedBuffer n . marshalBufferAccess

unmapBuffer :: MonadIO m => BufferObject -> m Bool
unmapBuffer (BufferObject n) = fmap unmarshalGLboolean . glUnmapNamedBuffer $ n

mapBufferRange_ :: MonadIO m => BufferObject -> BufferObjectOffset -> BufferObjectSize -> BufferObjectMapFlags -> m (Ptr ())
mapBufferRange_ (BufferObject obj) offset leng flags
  = glMapNamedBufferRange obj (fromIntegral offset) (fromIntegral leng) (marshalBufferObjectMapFlags flags)

mapBufferRange :: MonadIO m => BufferObject -> BufferObjectOffset -> BufferObjectSize -> BufferObjectMapFlags -> m (Maybe (Ptr ()))
mapBufferRange o off size flg = maybeNullPtr Nothing Just <$> mapBufferRange_ o off size flg

clearBufferSubData :: MonadIO m => BufferObject -> SizedFormat -> BufferObjectOffset -> BufferObjectSize -> SizedFormat -> GLDataType -> Ptr () -> m ()
clearBufferSubData (BufferObject obj) internalForm offset size form typ
  = glClearNamedBufferSubData obj (marshalSizedFormat internalForm) (fromIntegral offset) (fromIntegral size) (marshalSizedFormat form) (marshalGLDataType typ)

copyBufferSubData :: MonadIO m => BufferObject -> BufferObject -> BufferObjectOffset -> BufferObjectOffset -> BufferObjectSize -> m ()
copyBufferSubData (BufferObject readB) (BufferObject writeB) readOff writeOff size
  = glCopyNamedBufferSubData readB writeB (fromIntegral readOff) (fromIntegral writeOff) (fromIntegral size)

-- void glBindBufferRange(GLenum target​, GLuint index​, GLuint buffer​, GLintptr offset​, GLsizeiptr size​ );

--  void glBindBufferBase(GLenum target​, GLuint index​, GLuint buffer​);

bindBufferRange :: MonadIO m => BufferBindPoint -> BufferObjectIndex -> BufferObject -> BufferObjectOffset -> BufferObjectSize -> m ()
bindBufferRange pt (BufferObjectIndex n) (BufferObject m) (BufferObjectOffset l) (BufferObjectSize k) = glBindBufferRange (marshalBufferBindPoint pt) n m l k

bindBufferBase :: MonadIO m => BufferBindPoint -> BufferObjectIndex -> BufferObject -> m ()
bindBufferBase pt (BufferObjectIndex n) (BufferObject m) = glBindBufferBase (marshalBufferBindPoint pt) n m

-- Very unsafe! Respect alignment!
pokeBufferObject :: (Storable a, MonadIO m) => BufferObject -> VS.Vector a -> m ()
pokeBufferObject bo dat = liftIO . VS.unsafeWith dat $ bufferSubData bo (fromIntegral $ VS.length dat) 0 . castPtr

data PersistentBuffer a = PersistentBuffer
  { _getPersistentBufferPtr    :: Ptr a
  , _getPersistentBufferName   :: BufferObject
  , _getPersistentBufferSync   :: GLsync
  } deriving (Eq, Ord, Show)

persistentBufferFlag :: GLenum
persistentBufferFlag = GL_MAP_COHERENT_BIT .|. GL_MAP_PERSISTENT_BIT .|. GL_MAP_WRITE_BIT

genPersistentBufferArray :: forall a m. (Storable a, MonadIO m) => Int -> m (Maybe (PersistentBuffer a))
genPersistentBufferArray n = do
  bufo@(BufferObject ident) <- genObjectName
  glNamedBufferStorage ident size nullPtr persistentBufferFlag
  ptr <- glMapNamedBufferRange ident 0 size persistentBufferFlag
  return $ (\x -> PersistentBuffer (castPtr x) bufo nullPtr) <$> maybeNullPtr Nothing Just ptr
  where
    size = fromIntegral $ n * sizeOf (error "how are you seeing this" :: a)

genPersistentBuffer :: (Storable a, MonadIO m) => m (Maybe (PersistentBuffer a))
genPersistentBuffer = genPersistentBufferArray 1

fencePersistentBuffer :: MonadIO m => PersistentBuffer a -> m (PersistentBuffer a)
fencePersistentBuffer pb = do
  sync <- lockGLFence (_getPersistentBufferSync pb)
  return pb { _getPersistentBufferSync = sync}

persistentBufferWrite :: (Storable a, MonadIO m) => Word64 -> a -> PersistentBuffer a -> m ()
persistentBufferWrite timeout a PersistentBuffer {..} = liftIO $ do
  waitGLFence timeout _getPersistentBufferSync
  poke _getPersistentBufferPtr a

persistentBufferWriteArray :: forall a m. (Storable a, MonadIO m) => Word64 -> VS.Vector a -> PersistentBuffer a -> m ()
persistentBufferWriteArray timeout vec PersistentBuffer {..} = liftIO $ do
  waitGLFence timeout _getPersistentBufferSync
  VS.unsafeWith vec $ \srcptr -> copyBytes _getPersistentBufferPtr srcptr (VS.length vec * sizeOf (error "how?" :: a))
