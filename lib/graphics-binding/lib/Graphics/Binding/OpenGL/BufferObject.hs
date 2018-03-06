{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Binding.OpenGL.BufferObject where

import           Graphics.Binding.OpenGL.Synchro
import           Foreign
import           Graphics.Binding.OpenGL.Utils
import           Graphics.GL.Core45
import           Graphics.Binding.OpenGL.Types
import           Foreign.Resource
import           Data.Typeable
import           Control.Lens

initBufferObject :: MonadIO m
                 => BufferObjectSize
                 -> BufferObjectAttribFlags
                 -> Ptr ()
                 -> m BufferObject
initBufferObject size attrib@BufferObjectAttribFlags {..} ptr = do
  bufo@(BufferObject n) <- genName'
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

bindBufferRange :: MonadIO m => BufferBindPoint -> BufferObjectIndex -> BufferObject -> BufferObjectOffset -> BufferObjectSize -> m ()
bindBufferRange pt (BufferObjectIndex n) (BufferObject m) (BufferObjectOffset l) (BufferObjectSize k) = glBindBufferRange (marshalBufferBindPoint pt) n m l k

bindBufferBase :: MonadIO m => BufferBindPoint -> BufferObjectIndex -> BufferObject -> m ()
bindBufferBase pt (BufferObjectIndex n) (BufferObject m) = glBindBufferBase (marshalBufferBindPoint pt) n m

mapSizedBufferRange_ :: MonadIO m => SizedBufferObject -> BufferObjectOffset -> BufferObjectSize -> BufferObjectMapFlags -> m (Ptr ())
mapSizedBufferRange_ (SizedBufferObject obj) offset leng flags
  = glMapNamedBufferRange obj (fromIntegral offset) (fromIntegral leng) (marshalBufferObjectMapFlags flags)

unmapSizedBuffer :: MonadIO m => SizedBufferObject -> m Bool
unmapSizedBuffer (SizedBufferObject n) = fmap unmarshalGLboolean . glUnmapNamedBuffer $ n

bindSizedBufferRange :: MonadIO m => BufferBindPoint -> BufferObjectIndex -> SizedBufferObject -> BufferObjectOffset -> BufferObjectSize -> m ()
bindSizedBufferRange pt (BufferObjectIndex n) (SizedBufferObject m) (BufferObjectOffset l) (BufferObjectSize k) = glBindBufferRange (marshalBufferBindPoint pt) n m l k

instance GLWritable a => ForeignName (PersistentBuffer a) () where
  genName_ _ = do
    bufo <- genName (size, persistentBufferObjectFlag)
    ptr <- mapSizedBufferRange_ bufo 0 size persistentBufferMapFlag
    return $ PersistentBuffer (castPtr ptr) bufo nullPtr
      where
        size = fromIntegral $ gSize (Proxy :: Proxy a)

  isName_ (PersistentBuffer _ bufo _) = isName_ bufo

  deleteName_ (PersistentBuffer bufPtr bufo lock) = do
    deleteFence lock
    _ <- unmapSizedBuffer bufo
    deleteName_ bufo

fencePersistentBuffer :: MonadIO m => PersistentBuffer a -> m (PersistentBuffer a)
fencePersistentBuffer pb = do
  sync <- lockGLFence (_getPersistentBufferSync pb)
  return pb { _getPersistentBufferSync = sync}

persistentBufferWrite :: (GLWritable a, MonadIO m) => Word64 -> a -> PersistentBuffer a -> m ()
persistentBufferWrite timeout a PersistentBuffer {..} = liftIO $ do
  waitGLFence timeout _getPersistentBufferSync
  gPoke _getPersistentBufferPtr a

instance GLWritable a => ForeignName (WritableBuffer a) () where
  genName_ _ = WritableBuffer <$> genName (size, attribflags)
    where
      size = fromIntegral $ gSize (Proxy :: Proxy a)
      attribflags = defaultBufferAttribFlags & mapDynamic .~ True

  isName_ (WritableBuffer bufo) = isName_ bufo

  deleteName_ (WritableBuffer bufo) = deleteName_ bufo

writableBufferWrite :: forall a m. (GLWritable a, MonadIO m) => a -> WritableBuffer a -> m ()
writableBufferWrite foo (WritableBuffer (SizedBufferObject n))= liftIO $ allocaBytes size $ \ptr -> do
  gPoke ptr foo
  glNamedBufferSubData n 0 (fromIntegral size) (castPtr ptr)
  where
    size = gSize (Proxy :: Proxy a)
