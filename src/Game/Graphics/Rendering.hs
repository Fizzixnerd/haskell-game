{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
module Game.Graphics.Rendering where

import Unsafe.Coerce
import           ClassyPrelude
import           Control.Lens
import qualified Data.Vector.Storable         as VS
import           Foreign
import           Foreign.C.Types
import           Game.Types
import           Game.Graphics.OpenGL.Binding
import           Game.Entity.Player
import           Linear
import qualified Graphics.Rendering.OpenGL.GL as GL (DataType(..))

data UniformMVP = UniformMVP deriving (Eq, Ord, Show)

instance Uniform UniformMVP where
  type UniformContents UniformMVP = M44 GLfloat
  type UniformLocationType UniformMVP = DefaultBlock
  uniformLocation _    = makeGettableStateVar (return (UniformLocation 0))
  uniform prg _ = makeSettableStateVar $
    \mat -> primMarshall prg (UniformLocation 0) mat

render :: (TextureTarget t, MonadIO m)
       => GameState
       -> Program
       -> TextureUnit
       -> VertexArrayObject
       -> Int -- ^ How much of the VAO we want to draw
       -> TextureObject t
       -> m ()
render gs prog texSampleLoc vao n tex = liftIO $ do
  clear [ColorBuffer, DepthBuffer]
  currentProgram $= Just prog
  currentVertexArrayObject $= Just vao
  bindTextureUnit texSampleLoc tex
  cameraMatrix <- getPlayerOpenGLMatrix $ (gs ^. gameStatePlayer)
  uniform prog UniformMVP $= (unsafeCoerce cameraMatrix) -- coercing
                                                         -- from
                                                         -- cfloat ->
                                                         -- float
  drawElements Triangles (fromIntegral n) GL.UnsignedInt nullPtr

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
    ebuf <- initBufferObject (fromIntegral $ vecLen * sizeOf (undefined :: CUInt)) flags (castPtr indxs)
    return (ebuf, vecLen)

  bindElementBuffer vao ebuf

  return (vao, vbuf, (ebuf, lene))
