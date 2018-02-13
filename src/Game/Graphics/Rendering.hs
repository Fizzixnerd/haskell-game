{-# LANGUAGE NoImplicitPrelude #-}

module Game.Graphics.Rendering where

import           ClassyPrelude
import           Control.Lens
import qualified Data.Vector.Storable            as VS
import           Foreign
import           Foreign.C.Types
import           Game.Types
import           Game.Graphics.OpenGL.LowBinding
import qualified Graphics.Rendering.OpenGL.GL    as G (DataType(..))

render :: (TextureTarget t, MonadIO m) =>
          GameState
       -> Program
       -> UniformLocation
       -> TextureUnit
       -> VertexArrayObject
       -> Int -- ^ How much of the VAO we want to draw
       -> TextureObject t
       -> m ()
render gs prog mvpLoc texSampleLoc vao n tex = liftIO $ do
  clear [ColorBuffer, DepthBuffer]
  useProgram prog
  bindVertexArrayObject vao
  bindTextureUnit texSampleLoc tex
  uniform mvpLoc $= (gs ^. gameStateCamera . cameraMVP)
  drawElements Triangles (fromIntegral n) G.UnsignedInt nullPtr

unsafeWithVecLen :: (Storable a, MonadIO m) => VS.Vector a -> (Ptr a -> Int -> IO b) -> m b
unsafeWithVecLen vec f = liftIO $ do
  let (fptr, vecLen) = VS.unsafeToForeignPtr0 vec
  withForeignPtr fptr $ \ptr -> f ptr vecLen

bufferData :: MonadIO m
           => AttribLocation
           -> AttribLocation
           -> AttribLocation
           -> VS.Vector VTNPoint
           -> VS.Vector CUInt
           -> m (VertexArrayObject, BufferObject, (BufferObject, Int))
bufferData vtxLoc texLoc nmlLoc objPoints objIndices = liftIO $ do
  vao <- genObjectName
  let flags = defaultBufferAttribFlags & mapType .~ MapReadWrite & mapPersistent .~ True & mapCoherent .~ True
  vbuf <- unsafeWithVecLen objPoints $ \vtxs vecLen ->
    initBufferObject (toBufferObjectSize $ vecLen * fromIntegral (sizeOf (undefined :: VTNPoint))) flags (castPtr vtxs)
  let vtxOffset = toBufferObjectOffset (0 :: Int)
      texOffset = toBufferObjectOffset $ 4 * sizeOf (0 :: CFloat)
      nmlOffset = toBufferObjectOffset $ 6 * sizeOf (0 :: CFloat)
      stride    = toBufferObjectStride $ sizeOf (undefined :: VTNPoint)
      relOffset = BufferObjectRelOffset 0

  vertexArrayAttribEnable vao vtxLoc
  vertexArrayAttribFormat vao vtxLoc (BufferObjectComponentSize 4) GLFloat NotNormalized relOffset
  vertexArrayVertexBuffer vao vtxLoc vbuf vtxOffset stride
  vertexArrayAttribBinding vao vtxLoc vtxLoc

  vertexArrayAttribEnable vao texLoc
  vertexArrayAttribFormat vao texLoc (BufferObjectComponentSize 2) GLFloat NotNormalized relOffset
  vertexArrayVertexBuffer vao texLoc vbuf texOffset stride
  vertexArrayAttribBinding vao texLoc texLoc

  vertexArrayAttribEnable vao nmlLoc
  vertexArrayAttribFormat vao nmlLoc (BufferObjectComponentSize 3) GLFloat NotNormalized relOffset
  vertexArrayVertexBuffer vao nmlLoc vbuf nmlOffset stride
  vertexArrayAttribBinding vao nmlLoc nmlLoc


  (ebuf, lene) <- unsafeWithVecLen objIndices $ \indxs vecLen -> do
    ebuf <- initBufferObject (toBufferObjectSize $ vecLen * sizeOf (undefined :: CUInt)) flags (castPtr indxs)
    return (ebuf, vecLen)

  vertexArrayElementBuffer vao ebuf

  return (vao, vbuf, (ebuf, lene))
