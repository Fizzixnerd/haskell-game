{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Graphics.Model.AssImp where

import ClassyPrelude as ClassyP
import Control.Lens
import qualified Asset.AssImp.Types as A
import qualified Asset.AssImp.Import as A
import Foreign.C.Types
import qualified Data.Map as M
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Game.Graphics.Types
import Graphics.Binding
import Control.Monad (mfilter)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Linear as L
import Foreign.Resource

importAssImpFileGood :: FilePath -> IO A.ScenePtr
importAssImpFileGood = A.importAndProcessFileGood

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

peekV3 :: (A.MeshPtr -> IO (Ptr a)) -> A.MeshPtr -> IO (Vector (L.V3 Float))
peekV3 f meshPtr = do
  numVert <- A.meshNumVertices meshPtr
  ptr <- f meshPtr
  foldM (\vs i -> do
            let offset = fromIntegral i * 3 * sizeOf (0 :: Float)
            v <- A.peekVector3D $ castPtr $ ptr `plusPtr` offset
            return $ vs `V.snoc` v)
    V.empty [0..numVert-1]

peekVertices :: A.MeshPtr -> IO (Vector (L.V3 Float))
peekVertices = peekV3 A.meshVertices

peekNormals :: A.MeshPtr -> IO (Vector (L.V3 Float))
peekNormals = peekV3 A.meshNormals

peekTangents :: A.MeshPtr -> IO (Vector (L.V3 Float))
peekTangents = peekV3 A.meshTangents

addBoneWeight :: (Eq a, Num a) => a -> L.V4 a -> L.V4 a
addBoneWeight bd (L.V4 0 0 0 0) = L.V4 bd 0 0 0
addBoneWeight bd (L.V4 x 0 0 0) = L.V4 x bd 0 0
addBoneWeight bd (L.V4 x y 0 0) = L.V4 x y bd 0
addBoneWeight bd (L.V4 x y z 0) = L.V4 x y z bd
addBoneWeight _ _ = error "Attempt to addBoneWeight to full Bone datum!"

addBoneBoneID :: (Eq a, Num a) => a -> L.V4 a -> L.V4 a
addBoneBoneID bd (L.V4 (-1) (-1) (-1) (-1)) = L.V4 bd (-1) (-1) (-1)
addBoneBoneID bd (L.V4 x (-1) (-1) (-1)) = L.V4 x bd (-1) (-1)
addBoneBoneID bd (L.V4 x y (-1) (-1)) = L.V4 x y bd (-1)
addBoneBoneID bd (L.V4 x y z (-1)) = L.V4 x y z bd
addBoneBoneID _ _ = error "Attempt to addBoneBoneID to full Bone datum!"

peekVertexAttributes :: BoneIDMap -> Vector Bone -> A.MeshPtr -> IO (Vector AssImpVertex)
peekVertexAttributes _ bones meshPtr = do
  vs <- peekVertices meshPtr
  ns <- peekNormals meshPtr
  ts <- peekTangents meshPtr
  let initBoneIDs :: Vector (L.V4 Int) = replicate (length vs) (L.V4 (-1) (-1) (-1) (-1))
      initBoneWeights :: Vector (L.V4 Float) = replicate (length vs) (L.V4 0 0 0 0)
  (boneIDs, boneWeights_) <-
    foldM (\(bids, bws) b -> do
              let bid = fromIntegral $ b ^. boneID
              foldM (\(bids_, bws_) (vertexID, weight) -> do
                        let bids' = bids_ & ix vertexID %~ addBoneBoneID bid
                            bws'  = bws_ & ix vertexID %~ addBoneWeight weight
                        return (bids', bws'))
                (bids, bws) (b ^. boneWeights))
    (initBoneIDs, initBoneWeights) bones
  return $ foldl' (\vertices i ->
            let v = vs V.! i
                n = ns V.! i
                t = ts V.! i
                bid = boneIDs V.! i
                bw = boneWeights_ V.! i
            in
              vertices `V.snoc` AssImpVertex v (n ^. L._xy) t bid bw)
    empty [0..length vs - 1]

-- Layout:
-- 0: Vertex
-- 1: Normal
-- 2: Tangent
-- 3: BoneID
-- 4: BoneWeight
-- 5-13: Textures
massageAssImpMesh :: BoneIDMap -> Vector Bone -> A.MeshPtr -> IO (VS.Vector Float, Word32, Ptr Word32, Word32, Vector Word32, Vector Word32, Vector AssImpVertex)
massageAssImpMesh bm bv ptr = do
  numV    <- (\(CUInt x) -> x) <$> A.meshNumVertices ptr
  numUVs  <- castPtr <$> A.meshNumUVComponents ptr :: IO (Ptr Word32)
  vptr    <- castPtr <$> A.meshVertices ptr :: IO (Ptr Float)
  nptr    <- castPtr <$> A.meshNormals ptr
  tptr    <- castPtr <$> A.meshTangents ptr
  tptrptr <- A.meshTextureCoords ptr
  (faceptr, faceNum) <- A.bufferFaces ptr
  uvs_   <- V.fromList <$> peekArray 8 numUVs
  tptrs_ <- fmap castPtr . V.fromList <$> peekArray 8 tptrptr

  vertices <- peekVertexAttributes bm bv ptr

  let ptrs_       = V.fromList [vptr, nptr, tptr] <> tptrs_
      components_ = V.fromList [3, 3, 3] <> uvs_
      combined_   = V.filter ((/=0) . fst) $ V.zip components_ ptrs_
      (components, ptrs) = (fmap fst combined_, fmap snd combined_)
      offsets = V.prescanl' (+) 0 components
      chunkLen = fromIntegral $ sum components

  vdata <- rawInterleave (fromIntegral numV) chunkLen (V.zip3 ptrs components offsets)
  return (vdata, fromIntegral chunkLen, castPtr faceptr, fromIntegral faceNum, components, offsets, vertices)

loadBonesFromMesh :: A.MeshPtr -> IO (BoneIDMap, Vector Bone)
loadBonesFromMesh meshPtr = do
  numBones <- A.meshNumBones meshPtr
  meshBones_ <- A.meshBones meshPtr
  bm <- foldM (\bMap boneID_ -> do
                  let boneID' = fromIntegral boneID_
                  bonePtr <- peekElemOff meshBones_ boneID'
                  name <- fromString <$> A.peekAIString (A.boneName bonePtr)
                  matrix <- A.peekMatrix4x4 $ A.boneOffsetMatrix bonePtr
                  let weights = A.boneWeights bonePtr
                  numWeights <- A.boneNumWeights bonePtr
                  weightsVector <- V.generateM (fromIntegral numWeights) $ \i -> do
                    vw <- A.peekVertexWeight $ weights `plusPtr` (A.sizeOfVertexWeight * i)
                    return (A.vertexWeightVID vw, A.vertexWeightVWeight vw)
                  let bone = Bone
                             { _boneName = name
                             , _boneID = boneID'
                             , _boneMatrix = matrix
                             , _boneWeights = weightsVector
                             }
                  return $ M.insert name bone bMap)
        M.empty [0..numBones-1]
  let bv = sortOn _boneID $ fromList $ toList bm
  return (_boneID <$> bm, bv)

marshalAssImpMesh :: A.ScenePtr -> A.MeshPtr -> IO AssImpMesh
marshalAssImpMesh sc ptr = do
  (boneIDMap, boneVector') <- loadBonesFromMesh ptr
  (vdata, chunkLen, faceptr, faceNum, uvs, texOffsets, _) <-
    massageAssImpMesh boneIDMap boneVector' ptr

  let texData = V.zip uvs . fmap ((* sizeOf (0 :: CFloat)) . fromIntegral) $ texOffsets
      flags   = defaultBufferAttribFlags
      buffInit size_ ptr_ = initBufferName size_ flags (castPtr ptr_)
      stride = sizeOf (0 :: CFloat) * fromIntegral chunkLen

  numAnimations <- fromIntegral <$> A.sceneNumAnimations sc

  animationsPtrPtr <- A.sceneAnimations sc

  animationPtrVector <- V.generateM numAnimations $ \i -> peekElemOff animationsPtrPtr i
  animationMap <- foldM (\aMap animationPtr -> do
                            animation <- peekAnimation animationPtr
                            let name = animation ^. animationName
                            return $ M.insert name animation aMap)
                  M.empty animationPtrVector
  let animationIDMap = M.fromList $ imap (\i (x, _) -> (x, i)) $ M.toList animationMap
      animationVector :: Vector Animation = fromList $ toList animationMap
  boneAnims <- foldM (\animations Animation {..} -> do
                         boneAnim <- foldM (\boneAnimations NodeAnim {..} -> do
                                               let _boneAnimationName = _animationName
                                                   _boneAnimationPositions = _nodeAnimPositionKeys
                                                   _boneAnimationRotations = _nodeAnimRotationKeys
                                                   _boneAnimationScalings  = _nodeAnimScalingKeys
                                                   _boneAnimationDuration  = _animationDuration
                                                   _boneAnimationTicksPerSecond = _animationTicksPerSecond
                                               return $ boneAnimations `V.snoc` BoneAnimation {..})
                                     empty _animationChannels
                         return $ animations `V.snoc` boneAnim)
               empty animationVector
  -- Now we transpose boneAnims
  let boneAnims' :: Vector (Vector BoneAnimation) =
        fromList $ transposeOf traverse $ toList <$> boneAnims
      boneVector = zip boneVector' boneAnims'

  vao <- genName'
  vbuf <- VS.unsafeWith vdata $ \vptr -> buffInit
    (fromIntegral $ length vdata * sizeOf (0 :: CFloat)) vptr
  ibuf <- buffInit (fromIntegral $ fromIntegral faceNum * sizeOf (0 :: CUInt)) faceptr

  vertexArrayVertexBuffer vao 0 vbuf 0 (fromIntegral stride)
  let fullAttribInit loc_ numComponents_ offset_ = do
        vertexArrayAttribFormat vao loc_ numComponents_ GLFloat NotNormalized offset_
        vertexArrayAttribCapability vao loc_ Enabled
        vertexArrayAttribBinding vao loc_ 0

  V.imapM_ (\i (uv, offset) -> fullAttribInit (fromIntegral i) (fromIntegral uv) (fromIntegral offset)) texData
  bindElementBuffer vao ibuf

  mats <- A.sceneMaterials sc
  idx <- A.meshMaterialIndex ptr

  mat <- peekElemOff mats (fromIntegral idx)

  let matTex = fmap (mfilter $ not . null) . A.materialTexture mat
  diffuseName      <- matTex A.TextureTypeDiffuse
  specularName     <- matTex A.TextureTypeSpecular
  ambientName      <- matTex A.TextureTypeAmbient
  emmisiveName     <- matTex A.TextureTypeEmmisive
  heightName       <- matTex A.TextureTypeHeight
  normalName       <- matTex A.TextureTypeNormals
  shininessName    <- matTex A.TextureTypeShininess
  opacityName      <- matTex A.TextureTypeOpacity
  displacementName <- matTex A.TextureTypeDisplacement
  lightMapName     <- matTex A.TextureTypeLightMap
  reflectionName   <- matTex A.TextureTypeReflection

  (L.V3 dr dg db) <- A.materialColorDiffuse mat
  let diffuseColor = L.V4 dr dg db 0
  (L.V3 ar ag ab) <- A.materialColorAmbient mat
  let ambientColor = L.V4 ar ag ab 0
  (L.V3 sr sg sb) <- A.materialColorSpecular mat
  let specularColor = L.V4 sr sg sb 0
  specularStrength <- A.materialShininessStrength mat
  specularExponent <- A.materialShininess mat

  return AssImpMesh
    { _assImpMeshVAO             = vao
    , _assImpMeshBufferName      = vbuf
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
    , _assImpMeshBones = boneVector
    , _assImpMeshBoneIDMap = boneIDMap
    , _assImpMeshAnimationIDMap = animationIDMap
    }

marshalAssImpScene :: A.ScenePtr -> IO AssImpScene
marshalAssImpScene sc = do
  numMeshes <- fromIntegral <$> A.sceneNumMeshes sc
  mptr <- A.sceneMeshes sc

  assImpMeshes_ <- V.generateM numMeshes $ peekElemOff mptr >=> marshalAssImpMesh sc
  return $ AssImpScene assImpMeshes_

loadAssImpScene :: MonadIO m => FilePath -> m AssImpScene
loadAssImpScene = liftIO . (importAssImpFileGood >=> marshalAssImpScene)
