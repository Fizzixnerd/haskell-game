{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Game.Graphics.Model.AssImp where

import ClassyPrelude as ClassyP
import Asset.AssImp.Types
import Asset.AssImp.Import
import Foreign.C.Types
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Utils
import Game.Types
import Control.Lens
import Graphics.Binding
import Data.Maybe (fromJust)
import Control.Monad (mfilter)
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
  numV    <- (\(CUInt x) -> x) <$> meshNumVertices ptr
  numUVs  <- castPtr <$> meshNumUVComponents ptr :: IO (Ptr Word32)
  vptr    <- castPtr <$> meshVertices ptr
  nptr    <- castPtr <$> meshNormals ptr
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

  mems <- mallocForeignPtrArray totalLen :: IO (ForeignPtr Float)
  withForeignPtr mems $ \memptr ->
    flip V.imapM_ ptrs $ \chunkIndex ptr_ ->
      forM_ [0..(numV-1)] $ \n -> do
        let componentNum = V.unsafeIndex components chunkIndex
            destPtr = plusPtr memptr $ (chunkLen * fromIntegral n + chunkIndex) * sizeOf (0 :: CFloat)
            srcPtr  = plusPtr ptr_ $ 3 * sizeOf (0 :: CFloat) * fromIntegral n
        copyBytes destPtr srcPtr (fromIntegral componentNum * sizeOf (0 :: CFloat))
  let vdata = VS.unsafeFromForeignPtr0 mems totalLen
  return (vdata, fromIntegral chunkLen, castPtr faceptr, fromIntegral faceNum, components, offsets)

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

  let matTex = fmap (mfilter (not . null) . Just) . materialTexture mat
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
