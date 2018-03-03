{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Game.Graphics.Model.AssImp where

import ClassyPrelude as ClassyP
import Asset.AssImp.Types
import Asset.AssImp.Import
import Foreign.C.Types
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Game.Types
import Graphics.Binding
import Control.Monad (mfilter)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

importAssImpFileGood :: FilePath -> IO ScenePtr
importAssImpFileGood = importAndProcessFileGood

rawInterleave :: (Storable a, Num a) => Int -> Vector (Ptr a) -> Vector Word32 -> Vector Word32 -> IO (VS.Vector a)
rawInterleave numV ptrs componentSizes offsets = do
  mems <- mallocForeignPtrArray totalLen
  withForeignPtr mems $ \memptr ->
    flip V.imapM_ ptrs $ \chunkIndex ptr_ ->
      let compSize   = fromIntegral $ V.unsafeIndex componentSizes chunkIndex
          elemOffset = fromIntegral $ V.unsafeIndex offsets chunkIndex
      in forM_ [0..(compSize -1)] $ \componentIndex ->
           forM_ [0..(numV-1)] $ \n -> do
             x <- peekElemOff ptr_ (3 * n + componentIndex)
             pokeElemOff memptr (chunkLen * n + componentIndex + elemOffset) x

  return $ VS.unsafeFromForeignPtr0 mems totalLen
  where
    chunkLen = fromIntegral $ sum componentSizes :: Int
    totalLen = chunkLen * numV

-- Layout:
-- 0: Vertex
-- 1: Texture channel 1
-- 2: Normal
-- 3+: remaining texture channels
massageAssImpMesh :: MeshPtr -> IO (VS.Vector Float, Word32, Ptr Word32, Word32, Vector Word32, Vector Word32)
massageAssImpMesh ptr = do
  numV    <- (\(CUInt x) -> x) <$> meshNumVertices ptr
  numUVs  <- castPtr <$> meshNumUVComponents ptr :: IO (Ptr Word32)
  vptr    <- castPtr <$> meshVertices ptr :: IO (Ptr Float)
  nptr    <- castPtr <$> meshNormals ptr
  tptrptr <- meshTextureCoords ptr
  (faceptr, faceNum) <- bufferFaces ptr
  uvs_   <- V.fromList <$> peekArray 8 numUVs
  tptrs_ <- fmap castPtr . V.fromList <$> peekArray 8 tptrptr

  let ptrs_       = V.cons vptr . V.cons nptr $ tptrs_
      components_ = V.cons 3 . V.cons 3 $ uvs_
      combined_   = V.filter ((/=0) . fst) $ V.zip components_ ptrs_
      (components, ptrs) = (fmap fst combined_, fmap snd combined_)
      offsets = V.prescanl' (+) 0 components
      chunkLen = fromIntegral $ sum components
  vdata <- rawInterleave (fromIntegral numV) ptrs components offsets

  return (vdata, chunkLen, castPtr faceptr, fromIntegral faceNum, components, offsets)

marshalAssImpMesh :: ScenePtr -> MeshPtr -> IO AssImpMesh
marshalAssImpMesh sc ptr = do
  (vdata, chunkLen, faceptr, faceNum, uvs, texOffsets) <- massageAssImpMesh ptr
  let texOffsets' = V.map ((* sizeOf (0 :: CFloat)) . fromIntegral) texOffsets

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

  V.imapM_ (\i (uv, offset) -> fullAttribInit (fromIntegral i) (fromIntegral uv) (fromIntegral offset)) texData
  bindElementBuffer vao ibuf

  mats <- sceneMaterials sc
  idx <- meshMaterialIndex ptr

  mat <- peekElemOff mats (fromIntegral idx)

  let matTex = fmap (mfilter $ not . null) . materialTexture mat
  diffuseName      <- matTex TextureTypeDiffuse
  specularName     <- matTex TextureTypeSpecular
  ambientName      <- matTex TextureTypeAmbient
  emmisiveName     <- matTex TextureTypeEmmisive
  heightName       <- matTex TextureTypeHeight
  normalName       <- matTex TextureTypeNormals
  shininessName    <- matTex TextureTypeShininess
  displacementName <- matTex TextureTypeDisplacement
  lightMapName     <- matTex TextureTypeLightMap
  reflectionName   <- matTex TextureTypeReflection

  return AssImpMesh
    { _assImpMeshVAO             = vao
    , _assImpMeshBufferObject    = vbuf
    , _assImpMeshTextureDetails  = uvs
    , _assImpMeshIndexBO         = ibuf
    , _assImpMeshIndexBOType     = UnsignedInt
    , _assImpMeshIndexNum        = faceNum
    , _assImpMeshDiffuseTexture  = diffuseName
    , _assImpMeshSpecularTexture = specularName
    , _assImpMeshAmbientTexture  = ambientName
    , _assImpMeshEmmisiveTexture = emmisiveName
    , _assImpMeshHeightTexture   = heightName
    , _assImpMeshNormalTexture   = normalName
    , _assImpMeshShininessTexture = shininessName
    , _assImpMeshDisplacementTexture = displacementName
    , _assImpMeshLightMapTexture = lightMapName
    , _assImpMeshReflectionTexture = reflectionName
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
