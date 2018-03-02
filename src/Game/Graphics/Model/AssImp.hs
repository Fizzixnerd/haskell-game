{-# LANGUAGE NoImplicitPrelude #-}

module Game.Graphics.Model.AssImp where

import ClassyPrelude as ClassyP
import Asset.AssImp.Types
import Asset.AssImp.Import
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import Game.Types
import Graphics.Binding
import qualified Data.Vector as V (generateM, empty, imapM_, unfoldrM)

importAssImpFileGood :: FilePath -> IO ScenePtr
importAssImpFileGood = importAndProcessFileGood

-- Layout:
-- 0: Vertex
-- 1: Texture channel 1
-- 2: Normal
-- 3+: remaining texture channels
marshalAssImpMesh :: MeshPtr -> IO AssImpMesh
marshalAssImpMesh ptr = do
  numV <- (\(CUInt x) -> x) <$> meshNumVertices ptr
  vptr <- meshVertices ptr
  nptr <- meshNormals ptr
  tptrptr <- meshTextureCoords ptr
  (faceptr, faceNum) <- bufferFaces ptr

  vao <- genObjectName
  let flags   = defaultBufferAttribFlags
      m       = fromIntegral $ sizeOf (0 :: CFloat)
      bufSize = fromIntegral $ 3 * m * numV
      buffInit ptr_ = initBufferObject bufSize flags (castPtr ptr_)

  vbuf <- buffInit vptr
  nbuf <-buffInit nptr
  tbufs <- flip V.unfoldrM 0 $ \i -> do
    tptr <- peekElemOff tptrptr i
    if tptr == nullPtr
      then return Nothing
      else buffInit tptr >>= (\x -> return $ Just (x, i+1))
  ibuf <- initBufferObject (fromIntegral $ fromIntegral faceNum * sizeOf (0 :: CUInt)) flags (castPtr faceptr)

  let (mtbuf1,tbufrest) = maybe (Nothing, V.empty) (first Just) $ ClassyP.uncons tbufs
      fullAttribInit loc_ buf_ = do
        vertexArrayAttribCapability vao loc_ Enabled
        vertexArrayAttribFormat vao loc_ 3 GLFloat NotNormalized 0
        vertexArrayVertexBuffer vao loc_ buf_ 0 (fromIntegral $ 3 * sizeOf (0 :: CFloat))
        vertexArrayAttribBinding vao loc_ loc_

  fullAttribInit 0 vbuf
  traverse_ (fullAttribInit 1) mtbuf1
  fullAttribInit 2 nbuf
  flip V.imapM_ tbufrest $ \i tbuf -> fullAttribInit (fromIntegral $ i+3) tbuf
  bindElementBuffer vao ibuf

  return AssImpMesh
    { _assImpMeshVAO = vao
    , _assImpMeshVertexBO = vbuf
    , _assImpMeshTextureBO = tbufs
    , _assImpMeshNormalBO = nbuf
    , _assImpMeshIndexBO = ibuf
    , _assImpMeshIndexBOType = UnsignedInt
    , _assImpMeshIndexNum = fromIntegral faceNum
    }

marshalAssImpScene :: ScenePtr -> IO AssImpScene
marshalAssImpScene sc = do
  numMeshes <- fromIntegral <$> sceneNumMeshes sc
  mptr <- sceneMeshes sc
  fmap AssImpScene $ V.generateM numMeshes $ peekElemOff mptr >=> marshalAssImpMesh

loadAssImpScene :: MonadIO m => FilePath -> m AssImpScene
loadAssImpScene = liftIO . (importAssImpFileGood >=> marshalAssImpScene)
