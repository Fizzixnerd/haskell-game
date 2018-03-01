{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
module Game.Graphics.Rendering where

import Unsafe.Coerce
import           ClassyPrelude
import           Control.Lens
import qualified Data.Vector.Storable         as VS
import           Foreign hiding (void)
import           Foreign.C.Types
import           Game.Types
import           Graphics.Binding
import           Game.Entity.Player
import           Game.Graphics.Shader.Loader
import           Game.Graphics.Texture.Loader
import           Game.Entity.Camera
import           Linear

render :: MonadIO m
       => GameState s
       -> Program
       -> VertexArrayObject
       -> Int -- ^ How much of the VAO we want to draw
       -> TextureObject TextureTarget2D
       -> m ()
render gs prog vao n tex = liftIO $ do
  clear $ defaultClearBuffer & clearBufferColor .~ True
                             & clearBufferDepth .~ True
  useProgram prog
  currentVertexArrayObject $= Just vao
  texture tex Simple2DSampler
  cameraMatrix <- cameraVP $ (gs ^. gameStateCamera)
  uniform prog UniformMVP cameraMatrix
  drawElements Triangles (fromIntegral n) UnsignedInt

unsafeWithVecLen :: (Storable a, MonadIO m) => VS.Vector a -> (Ptr a -> Int -> IO b) -> m b
unsafeWithVecLen vec f = liftIO $ do
  let (fptr, vecLen) = VS.unsafeToForeignPtr0 vec
  withForeignPtr fptr $ \ptr -> f ptr vecLen

bufferData :: MonadIO m
           => AttribLocation
           -> AttribLocation
           -> AttribLocation
           -> VS.Vector VTNPoint
           -> VS.Vector Word32
           -> m (VertexArrayObject, BufferObject, (BufferObject, Int))
bufferData vtxLoc texLoc nmlLoc objPoints objIndices = liftIO $ do
  vao <- genObjectName
  let flags = defaultBufferAttribFlags & mapType .~ MapReadWrite
                                       & mapPersistent .~ True
                                       & mapCoherent .~ True
  vbuf <- unsafeWithVecLen objPoints $ \vtxs vecLen ->
    initBufferObject (fromIntegral $ vecLen * (sizeOf (undefined :: VTNPoint))) flags (castPtr vtxs)
  let vtxOffset = 0
      texOffset = fromIntegral $ 4 * sizeOf (0 :: CFloat)
      nmlOffset = fromIntegral $ 6 * sizeOf (0 :: CFloat)
      stride    = fromIntegral $ sizeOf (undefined :: VTNPoint)
      relOffset = 0

  vertexArrayAttribCapability vao vtxLoc Enabled
  vertexArrayAttribFormat vao vtxLoc (BufferObjectComponentSize 4) GLFloat NotNormalized relOffset
  vertexArrayVertexBuffer vao vtxLoc vbuf vtxOffset stride
  vertexArrayAttribBinding vao vtxLoc vtxLoc

  vertexArrayAttribCapability vao texLoc Enabled
  vertexArrayAttribFormat vao texLoc (BufferObjectComponentSize 2) GLFloat NotNormalized relOffset
  vertexArrayVertexBuffer vao texLoc vbuf texOffset stride
  vertexArrayAttribBinding vao texLoc texLoc

  vertexArrayAttribCapability vao nmlLoc Enabled
  vertexArrayAttribFormat vao nmlLoc (BufferObjectComponentSize 3) GLFloat NotNormalized relOffset
  vertexArrayVertexBuffer vao nmlLoc vbuf nmlOffset stride
  vertexArrayAttribBinding vao nmlLoc nmlLoc

  (ebuf, lene) <- unsafeWithVecLen objIndices $ \indxs vecLen -> do
    ebuf <- initBufferObject (fromIntegral $ vecLen * sizeOf (0 :: CUInt)) flags (castPtr indxs)
    return (ebuf, vecLen)

  bindElementBuffer vao ebuf

  return (vao, vbuf, (ebuf, lene))

bufferDataAssImp :: MonadIO m
           => AttribLocation
           -> AttribLocation
           -> AttribLocation
           -> VS.Vector AssImpVertex
           -> VS.Vector Word32
           -> m (VertexArrayObject, BufferObject, (BufferObject, Int))
bufferDataAssImp vtxLoc texLoc nmlLoc objPoints objIndices = liftIO $ do
  vao <- genObjectName
  let flags = defaultBufferAttribFlags & mapType .~ MapReadWrite
                                       & mapPersistent .~ True
                                       & mapCoherent .~ True
  vbuf <- unsafeWithVecLen objPoints $ \vtxs vecLen ->
    initBufferObject (fromIntegral $ vecLen * (sizeOf (undefined :: VTNPoint))) flags (castPtr vtxs)
  let vtxOffset = 0
      texOffset = fromIntegral $ 3 * sizeOf (0 :: CFloat)
      nmlOffset = fromIntegral $ 5 * sizeOf (0 :: CFloat)
      stride    = fromIntegral $ sizeOf (undefined :: AssImpVertex)
      relOffset = 0

  vertexArrayAttribCapability vao vtxLoc Enabled
  vertexArrayAttribFormat vao vtxLoc (BufferObjectComponentSize 3) GLFloat NotNormalized relOffset
  vertexArrayVertexBuffer vao vtxLoc vbuf vtxOffset stride
  vertexArrayAttribBinding vao vtxLoc vtxLoc

  vertexArrayAttribCapability vao texLoc Enabled
  vertexArrayAttribFormat vao texLoc (BufferObjectComponentSize 2) GLFloat NotNormalized relOffset
  vertexArrayVertexBuffer vao texLoc vbuf texOffset stride
  vertexArrayAttribBinding vao texLoc texLoc

  vertexArrayAttribCapability vao nmlLoc Enabled
  vertexArrayAttribFormat vao nmlLoc (BufferObjectComponentSize 3) GLFloat NotNormalized relOffset
  vertexArrayVertexBuffer vao nmlLoc vbuf nmlOffset stride
  vertexArrayAttribBinding vao nmlLoc nmlLoc

  (ebuf, lene) <- unsafeWithVecLen objIndices $ \indxs vecLen -> do
    ebuf <- initBufferObject (fromIntegral $ vecLen * sizeOf (0 :: CUInt)) flags (castPtr indxs)
    return (ebuf, vecLen)

  bindElementBuffer vao ebuf

  return (vao, vbuf, (ebuf, lene))
