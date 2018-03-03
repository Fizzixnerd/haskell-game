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
import qualified Data.Vector as V
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
  uvs_ :: Vector Word32 <- flip V.unfoldrM 0 $ \i -> do
    let uvptr = numUVs `plusPtr` (i * sizeOf (0 :: CUInt))
    nullB <- peek (castPtr uvptr) :: IO Char
    if nullB == '\0'
      then return Nothing
      else peek uvptr >>= (\x -> return $ Just (x, i+1))
  tptrs_ <- V.generateM (length uvs_) $ fmap castPtr . peekElemOff tptrptr

  let insertAround x y v = V.cons x . V.cons (V.head v) . V.cons y $ V.tail v
      (ptrs, components, offsets) = if null uvs_
                                    then (V.fromList [vptr, nptr], V.fromList [3,3], V.fromList [0,3,6])
                                    else let uvs_' = insertAround 3 3 uvs_
                                         in ( insertAround vptr nptr tptrs_
                                            , uvs_'
                                            , V.scanl' (+) 0 uvs_')
      chunkLen = fromIntegral $ sum components
      totalLen = chunkLen * fromIntegral numV

      getData n = if uv_ == 2 && offs_ == 1
                  then (\x -> 1-x) <$> peekElemOff ptr_ place
                  else peekElemOff ptr_ place
        where
          (n', i) = n `divMod` fromIntegral chunkLen
          indx    = subtract 1 . fromMaybe (error "Index out of range in assimp") $  V.findIndex (> fromIntegral i) offsets
          ptr_    = V.unsafeIndex ptrs indx
          offs_   = i - fromIntegral (V.unsafeIndex offsets indx)
          uv_     = V.unsafeIndex components indx
          place   = 3 * n' + fromIntegral offs_

  vdata <- VS.generateM totalLen getData
  return (vdata, fromIntegral chunkLen, castPtr faceptr, fromIntegral faceNum, components, offsets)

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

marshalAssImpScene :: ScenePtr -> IO AssImpScene
marshalAssImpScene sc = do
  numMeshes <- fromIntegral <$> sceneNumMeshes sc
  mptr <- sceneMeshes sc
  fmap AssImpScene $ V.generateM numMeshes $ peekElemOff mptr >=> marshalAssImpMesh sc

loadAssImpScene :: MonadIO m => FilePath -> m AssImpScene
loadAssImpScene = liftIO . (importAssImpFileGood >=> marshalAssImpScene)
