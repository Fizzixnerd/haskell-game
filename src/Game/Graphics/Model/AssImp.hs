{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Game.Graphics.Model.AssImp where

import ClassyPrelude as ClassyP
import Asset.AssImp.Types
import Asset.AssImp.Import
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import Game.Types
import Control.Lens
import Graphics.Binding
import Data.Maybe (fromJust)
import qualified Data.Vector as V (generateM, imapM_, unfoldrM, prescanl, findIndex, zip)
import qualified Data.Vector.Storable as VS

importAssImpFileGood :: FilePath -> IO ScenePtr
importAssImpFileGood = importAndProcessFileGood

-- Layout:
-- 0: Vertex
-- 1: Texture channel 1
-- 2: Normal
-- 3+: remaining texture channels
massageAssImpMesh :: MeshPtr -> IO (VS.Vector Float, Word32, Ptr Word32, Word32, Vector Word32, Vector Word32)
massageAssImpMesh ptr = do
  numV <- (\(CUInt x) -> x) <$> meshNumVertices ptr
  numUVs <- castPtr <$> meshNumUVComponents ptr :: IO (Ptr Word32)
  vptr <- castPtr <$> meshVertices ptr
  nptr <- castPtr <$> meshNormals ptr
  tptrptr <- meshTextureCoords ptr
  (faceptr, faceNum) <- bufferFaces ptr
  uvs :: Vector Word32 <- flip V.unfoldrM 0 $ \i -> do
    let uvptr = numUVs `plusPtr` (i * sizeOf (0 :: CUInt))
    nullB <- peek (castPtr uvptr) :: IO Char
    if nullB == '\0'
      then return Nothing
      else peek uvptr >>= (\x -> return $ Just (x, i+1))
  tptrs <- V.generateM (length uvs) $ fmap castPtr . peekElemOff tptrptr

  let chunkLen = fromIntegral $ sum uvs + 6
      totalLen = chunkLen * fromIntegral numV

      texOffsets = ClassyP.snoc (V.prescanl (+) 0 uvs) (fromIntegral $ chunkLen - 6)

      tptrRange n = do
        i    <- subtract 1 <$> V.findIndex (> n) texOffsets
        tptr <- ClassyP.index tptrs i
        offs <- ClassyP.index texOffsets i
        uv   <- ClassyP.index uvs i
        return (n-offs, tptr, uv)

      getData n
        | i < 3     = peekElemOff vptr (3 * n'+i)
        | i < 6     = peekElemOff nptr (3 * n'+i-3)
        | otherwise = fromJust (tptrRange (fromIntegral $ i-6)) & (\(offs, tptr, uv) -> if uv == 2 && offs == 1
                                                                                        then (\x -> 1-x) <$> peekElemOff tptr (3*n' + fromIntegral offs) -- If textures are ever sane, remove this
                                                                                        else peekElemOff tptr (3*n' + fromIntegral offs))
        where
          (n', i) = n `divMod` chunkLen

  vdata <- VS.generateM totalLen getData
  return (vdata, fromIntegral chunkLen, castPtr faceptr, fromIntegral faceNum, uvs, texOffsets)

marshalAssImpMesh :: ScenePtr -> MeshPtr -> IO AssImpMesh
marshalAssImpMesh sc ptr = do
  (vdata, chunkLen, faceptr, faceNum, uvs, texOffsets) <- massageAssImpMesh ptr
  let texOffsets' = flip map texOffsets $ \n -> sizeOf (0 :: CFloat) * fromIntegral (6+n)

  vao <- genObjectName
  let flags   = defaultBufferAttribFlags
      buffInit size_ ptr_ = initBufferObject size_ flags (castPtr ptr_)
  vbuf <- VS.unsafeWith vdata $ \vptr -> buffInit (fromIntegral $ length vdata * sizeOf (0 :: CFloat)) vptr
  ibuf <- buffInit (fromIntegral $ fromIntegral faceNum * sizeOf (0 :: CUInt)) faceptr
  let
    stride = sizeOf (0 :: CFloat) * fromIntegral chunkLen
    texData = V.zip uvs texOffsets'
    fullAttribInit loc_ numComponents_ offset_ = do
      vertexArrayAttribCapability vao loc_ Enabled
      vertexArrayAttribFormat vao loc_ numComponents_ GLFloat NotNormalized 0
      vertexArrayVertexBuffer vao loc_ vbuf offset_ (fromIntegral stride)
      vertexArrayAttribBinding vao loc_ loc_

  fullAttribInit 0 3 0
  fullAttribInit 1 3 (fromIntegral $ 3 * sizeOf (0 :: Float))
  V.imapM_ (\i (uv, offset) -> fullAttribInit (fromIntegral $ i+2) (fromIntegral uv) (fromIntegral offset)) texData
  bindElementBuffer vao ibuf

  mats <- sceneMaterials sc
  nMats <- sceneNumMaterials sc
  idx <- meshMaterialIndex ptr

  mat <- peekElemOff mats (fromIntegral idx)
  (_, textureName_, _, _, _, _, _, _) <- getMaterialTexture mat TextureTypeDiffuse 0
  let textureName = if not $ null textureName_
                    then Just textureName_
                    else Nothing
    
  return AssImpMesh
    { _assImpMeshVAO             = vao
    , _assImpMeshBufferObject    = vbuf
    , _assImpMeshTextureDetails  = uvs
    , _assImpMeshIndexBO         = ibuf
    , _assImpMeshIndexBOType     = UnsignedInt
    , _assImpMeshIndexNum        = faceNum
    , _assImpMeshMaterialTexture = textureName
    }
{-
marshalAssImpMesh' :: MeshPtr -> IO AssImpMesh
marshalAssImpMesh' ptr = do
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
-}
marshalAssImpScene :: ScenePtr -> IO AssImpScene
marshalAssImpScene sc = do
  numMeshes <- fromIntegral <$> sceneNumMeshes sc
  mptr <- sceneMeshes sc
  fmap AssImpScene $ V.generateM numMeshes $ peekElemOff mptr >=> marshalAssImpMesh sc

loadAssImpScene :: MonadIO m => FilePath -> m AssImpScene
loadAssImpScene = liftIO . (importAssImpFileGood >=> marshalAssImpScene)
