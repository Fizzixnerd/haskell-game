{-# LANGUAGE NoImplicitPrelude #-}

module Game.Graphics.OpenGL.Utils where

import ClassyPrelude
import Foreign hiding (void)
import qualified Data.Vector.Storable as VS
import Graphics.GL.Types
import qualified Data.ByteString.Internal as BI (create)
import qualified Data.ByteString.Unsafe as BU (unsafeUseAsCStringLen)

withForeignBufferVec :: (Storable a, MonadIO m) => Int -> (Ptr a -> IO ()) -> m (VS.Vector a)
withForeignBufferVec n f = liftIO $ do
  mems <- mallocForeignPtrArray n
  withForeignPtr mems f
  return $ VS.unsafeFromForeignPtr0 mems n

withForeignBufferBS :: (Storable n, Storable p, Integral n, Integral p, MonadIO m)
                    => (Ptr n -> IO ())
                    -> (p -> Ptr GLsizei -> Ptr GLchar -> IO ())
                    -> m ByteString
withForeignBufferBS lenAct fillAct = liftIO $ do
  len <- foreignPoke lenAct
  BI.create (fromIntegral len) $ \strptr -> void $ foreignPoke (\lenptr -> fillAct (fromIntegral len) lenptr (castPtr strptr))

foreignPoke :: (Storable a, MonadIO m) => (Ptr a -> IO ()) -> m a
foreignPoke act = liftIO . allocaArray 1 $ \ptr -> do
  act ptr
  peekElemOff ptr 0

withByteString :: ByteString -> (Ptr GLchar -> GLsizei -> IO b) -> IO b
withByteString bs act =
   BU.unsafeUseAsCStringLen bs $ \(ptr, size) ->
      act (castPtr ptr) (fromIntegral size)

