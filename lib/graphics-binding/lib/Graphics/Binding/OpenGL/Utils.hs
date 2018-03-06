{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Binding.OpenGL.Utils
  ( module X
  , withForeignBufferBS
  , withByteString
  , maybeNullPtr
  , foreignPoke
  , unsafeWithVecLen
  , fillFrozenM44CFloat
  , unsafeWithFrozenMatrix
  ) where

import Foreign as X
  ( Ptr
  , Storable
  , sizeOf
  , mallocForeignPtrArray
  , castPtr
  , nullPtr
  , with
  )
import           Control.Monad                      (void)
import           Control.Monad.IO.Class       as X
import           Data.ByteString
import qualified Data.ByteString.Internal     as BI (create)
import qualified Data.ByteString.Unsafe       as BU (unsafeUseAsCStringLen)
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as MS
import           Foreign                      hiding (void)
import           Foreign.C.Types
import           Graphics.GL.Types
import           Linear                              (M44)

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

maybeNullPtr :: b -> (Ptr a -> b) -> Ptr a -> b
maybeNullPtr n f ptr | ptr == nullPtr = n
                     | otherwise      = f ptr

unsafeWithVecLen :: (Storable a, MonadIO m, Integral n) => VS.Vector a -> (n -> Ptr a -> IO b) -> m b
unsafeWithVecLen vec act = liftIO $
  VS.unsafeWith vec $ \ptr -> act (fromIntegral n) ptr
  where
    n = VS.length vec
newtype FrozenM44 a  = FrozenM44 (ForeignPtr (M44 a))
  deriving (Eq, Ord, Show)

fillFrozenM44_ :: Storable a => Int -> (Ptr a -> IO b) -> IO (FrozenM44 a)
fillFrozenM44_ bytesize act = do
  mems <- mallocForeignPtrBytes (16 * bytesize) :: IO (ForeignPtr a)
  _ <- withForeignPtr mems act
  return $ FrozenM44 (castForeignPtr mems :: ForeignPtr (M44 a))

fillFrozenM44CFloat :: (Ptr CFloat -> IO b) -> IO (FrozenM44 CFloat)
fillFrozenM44CFloat = fillFrozenM44_ (sizeOf (0 :: CFloat))

unsafeWithFrozenMatrix :: forall a b. Storable a => FrozenM44 a -> (M44 a -> M44 a) -> (Ptr a -> IO b) -> IO ()
unsafeWithFrozenMatrix (FrozenM44 mtr) endo act = do
  let mvec = MS.unsafeFromForeignPtr0 mtr 1 :: MS.IOVector (M44 a)
  MS.unsafeModify mvec endo 1
  _ <- MS.unsafeWith mvec (act . \ptr -> castPtr ptr :: Ptr a)
  return ()
