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
import Foreign.Marshal.Utils
import Foreign.Ptr
import Game.Types
import Graphics.Binding
import Control.Monad (mfilter)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Linear as L

importAssImpFileGood :: FilePath -> IO ScenePtr
importAssImpFileGood = importAndProcessFileGood

rawInterleave :: forall a. (Storable a, Num a) => Int -> Int -> Vector (Ptr a, Word32, Word32) -> IO (VS.Vector a)
rawInterleave numV chunkLen ptrData = do
  mems <- mallocForeignPtrArray totalLen
  withForeignPtr mems $
    \memptr -> V.forM_ ptrData $
    \(ptr_, compSize, elemOffset) -> forM_ [0..(numV-1)] $
    \n -> let destPtr = plusPtr memptr $ elemSize * (chunkLen * n + fromIntegral elemOffset)
              srcPtr  = plusPtr ptr_ $ elemSize * (3 * fromIntegral n)
          in copyBytes destPtr srcPtr (fromIntegral compSize * fromIntegral elemSize)

  return $ VS.unsafeFromForeignPtr0 mems totalLen
  where
    elemSize = sizeOf (error "how are you seeing this?" :: a)
    totalLen = chunkLen * numV

-- Layout:
-- 0: Vertex
-- 1: Normal
-- 2+: texture channels
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

  vdata <- rawInterleave (fromIntegral numV) chunkLen (V.zip3 ptrs components offsets)
  return (vdata, fromIntegral chunkLen, castPtr faceptr, fromIntegral faceNum, components, offsets)

marshalAssImpMesh :: ScenePtr -> MeshPtr -> IO AssImpMesh
marshalAssImpMesh sc ptr = do
  (vdata, chunkLen, faceptr, faceNum, uvs, texOffsets) <- massageAssImpMesh ptr

  let texData = V.zip uvs . fmap ((* sizeOf (0 :: CFloat)) . fromIntegral) $ texOffsets
      flags   = defaultBufferAttribFlags
      buffInit size_ ptr_ = initBufferObject size_ flags (castPtr ptr_)
      stride = sizeOf (0 :: CFloat) * fromIntegral chunkLen

  vao <- genObjectName
  vbuf <- VS.unsafeWith vdata $ \vptr -> buffInit (fromIntegral $ length vdata * sizeOf (0 :: CFloat)) vptr
  ibuf <- buffInit (fromIntegral $ fromIntegral faceNum * sizeOf (0 :: CUInt)) faceptr

  vertexArrayVertexBuffer vao 0 vbuf 0 (fromIntegral stride)
  let fullAttribInit loc_ numComponents_ offset_ = do
        vertexArrayAttribFormat vao loc_ numComponents_ GLFloat NotNormalized offset_
        vertexArrayAttribCapability vao loc_ Enabled
        vertexArrayAttribBinding vao loc_ 0

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
  opacityName      <- matTex TextureTypeOpacity
  displacementName <- matTex TextureTypeDisplacement
  lightMapName     <- matTex TextureTypeLightMap
  reflectionName   <- matTex TextureTypeReflection

  (Color3D dr dg db) <- materialColorDiffuse mat
  let diffuseColor = L.V4 dr dg db 0
  (Color3D ar ag ab) <- materialColorAmbient mat
  let ambientColor = L.V4 ar ag ab 0
  (Color3D sr sg sb) <- materialColorSpecular mat
  let specularColor = L.V4 sr sg sb 0
  specularStrength <- materialShininessStrength mat
  specularExponent <- materialShininess mat



  return AssImpMesh
    { _assImpMeshVAO             = vao
    , _assImpMeshBufferObject    = vbuf
    , _assImpMeshTextureDetails  = uvs
    , _assImpMeshIndexBO         = ibuf
    , _assImpMeshIndexBOType     = UnsignedInt
    , _assImpMeshIndexNum        = faceNum
    , _assImpMeshTextureBundle   = TextureBundle
      { _textureBundleDiffuseTexture      = diffuseName
      , _textureBundleSpecularTexture     = specularName
      , _textureBundleAmbientTexture      = ambientName
      , _textureBundleEmmisiveTexture     = emmisiveName
      , _textureBundleHeightTexture       = heightName
      , _textureBundleNormalTexture       = normalName
      , _textureBundleShininessTexture    = shininessName
      , _textureBundleOpacityTexture      = opacityName
      , _textureBundleDisplacementTexture = displacementName
      , _textureBundleLightMapTexture     = lightMapName
      , _textureBundleReflectionTexture   = reflectionName
      }
    , _assImpMeshShaderMaterial = ShaderMaterial
      { _shaderMaterialDiffuseColor = diffuseColor
      , _shaderMaterialAmbientColor = ambientColor
      , _shaderMaterialSpecularColor = specularColor
      , _shaderMaterialSpecularStrength = specularStrength
      , _shaderMaterialSpecularExponent = specularExponent
      }
    }

marshalAssImpScene :: ScenePtr -> IO AssImpScene
marshalAssImpScene sc = do
  numMeshes <- fromIntegral <$> sceneNumMeshes sc
  mptr <- sceneMeshes sc
  fmap AssImpScene $ V.generateM numMeshes $ peekElemOff mptr >=> marshalAssImpMesh sc

loadAssImpScene :: MonadIO m => FilePath -> m AssImpScene
loadAssImpScene = liftIO . (importAssImpFileGood >=> marshalAssImpScene)
