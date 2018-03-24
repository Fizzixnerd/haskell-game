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
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Game.Graphics.Types
import Graphics.Binding
import Control.Monad (mfilter)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Linear as L
import Foreign.Resource
import Text.Printf

importAssImpFileGood :: FilePath -> IO A.ScenePtr
importAssImpFileGood = A.importAndProcessFileGood

-- Note that this function takes a "dynamic" sizeOfer where it WILL INSPECT THE
-- ARGUMENT. However, it will assume all elements in the Vector as the same
-- size. Returns a nullPtr when v is empty.
interleaveWith :: (Ptr a -> a -> IO ()) -> (a -> Int) -> Vector a -> IO (Ptr a)
interleaveWith poker sizeOfer v =
  if null v
  then return nullPtr
  else do
    let sizeOfA = sizeOfer $ v V.! 0
    ptr <- mallocBytes $ sizeOfA * length v
    forM_ [0 .. length v - 1] $ \i -> poker (ptr `plusPtr` (i * sizeOfA)) (v V.! i)
    return ptr

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
  numVert <- fromIntegral <$> A.meshNumVertices meshPtr
  ptr <- f meshPtr
  V.generateM numVert $ \i -> do
    let offset = i * A.sizeOfVector3D
    A.peekVector3D $ castPtr $ ptr `plusPtr` offset

peekVertices :: A.MeshPtr -> IO (Vector (L.V3 Float))
peekVertices = peekV3 A.meshVertices

peekNormals :: A.MeshPtr -> IO (Vector (L.V3 Float))
peekNormals = peekV3 A.meshNormals

peekTangents :: A.MeshPtr -> IO (Vector (L.V3 Float))
peekTangents = peekV3 A.meshTangents

addBoneWeight :: Float -> L.V4 Float -> L.V4 Float
addBoneWeight bd (L.V4 0 0 0 0) = L.V4 bd 0 0 0
addBoneWeight bd (L.V4 x 0 0 0) = L.V4 x bd 0 0
addBoneWeight bd (L.V4 x y 0 0) = L.V4 x y bd 0
addBoneWeight bd (L.V4 x y z 0) = L.V4 x y z bd
addBoneWeight _ x = trace "Attempt to addBoneWeight to full Bone datum!" x

addBoneID :: Int -> L.V4 Int -> L.V4 Int
addBoneID bd (L.V4 (-1) (-1) (-1) (-1)) = L.V4 bd (-1) (-1) (-1)
addBoneID bd (L.V4 x (-1) (-1) (-1)) = L.V4 x bd (-1) (-1)
addBoneID bd (L.V4 x y (-1) (-1)) = L.V4 x y bd (-1)
addBoneID bd (L.V4 x y z (-1)) = L.V4 x y z bd
addBoneID _ x = trace "Attempt to addBoneID to full Bone datum!" x

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
                        let bids' = bids_ & ix vertexID %~ addBoneID bid
                            bws'  = bws_ & ix vertexID %~ addBoneWeight weight
                        return (bids', bws'))
                (bids, bws) (b ^. boneWeights))
    (initBoneIDs, initBoneWeights) bones

  -- now the textures. First we make sure they conform to the [4 x 2, 2 x 3, 2 x 1]
  -- component framework.
  numUVs <- castPtr <$> A.meshNumUVComponents meshPtr
  numUVComponents :: Vector Word32 <- V.fromList <$> peekArray maxTextureChannels numUVs
  let errorOnComponents :: Int -> Word32 -> Int -> m ()
      errorOnComponents i n m = error $ printf "texture at index %s has %s components, expected 0 or %s." (show i) (show n) (show m)
  imapM_ (\idx num -> if 0 <= idx && idx < 4 then
                        when (num /= 0 && num /= 2) $
                        errorOnComponents idx num 2
                      else if 4 <= idx && idx < 6
                           then when (num /= 0 && num /= 3) $
                                errorOnComponents idx num 3
                           else if 6 <= idx && idx < 8
                                then when (num /= 0 && num /= 1) $
                                     errorOnComponents idx num 1
                                else error "Expected only 8 uv channels, but got more than that.") numUVComponents

  -- this will get a Just (Vector (L.V3 Float)) if the numComponents isn't 0.
  let getUV i = if numUVComponents V.! i == 0
                then return Nothing
                else do
        uvs <- peekV3 (\ptr -> do
                          tptrptr <- A.meshTextureCoords ptr
                          peekElemOff tptrptr i) meshPtr
        return $ Just uvs

  uv0'  <- getUV 0
  uv1'  <- getUV 1
  uv2'  <- getUV 2
  uv3'  <- getUV 3
  uvw0' <- getUV 4
  uvw1' <- getUV 5
  u0'   <- getUV 6
  u1'   <- getUV 7

  -- FIXME: This really should not sequence. I should change the types... Also
  -- apparently traverse = sequence . fmap? Alrighty...
  let uv0  = fmap (fmap $ \x -> x ^. L._xy) uv0'
      uv1  = fmap (fmap $ \x -> x ^. L._xy) uv1'
      uv2  = fmap (fmap $ \x -> x ^. L._xy) uv2'
      uv3  = fmap (fmap $ \x -> x ^. L._xy) uv3'
      uvw0 = uvw0'
      uvw1 = uvw1'
      u0   = fmap (fmap $ \x -> x ^. L._x)  u0'
      u1   = fmap (fmap $ \x -> x ^. L._x)  u1'

  return $ V.generate (length vs) $
    \i ->
      let (!!?) :: Maybe (Vector a) -> Int -> Maybe a
          x !!? j = do
            x' <- x
            return $ x' V.! j
          v = vs V.! i
          n = ns V.! i
          t = ts V.! i
          bid = boneIDs V.! i
          bw = boneWeights_ V.! i
          uv0_ = uv0 !!? i
          uv1_ = uv1 !!? i
          uv2_ = uv2 !!? i
          uv3_ = uv3 !!? i
          uvw0_ = uvw0 !!? i
          uvw1_ = uvw1 !!? i
          u0_ = u0 !!? i
          u1_ = u1 !!? i
      in
        AssImpVertex v n t bid bw uv0_ uv1_ uv2_ uv3_ uvw0_ uvw1_ u0_ u1_

maxTextureChannels :: Int
maxTextureChannels = 8

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
  uvs_   <- V.fromList <$> peekArray maxTextureChannels numUVs
  tptrs_ <- fmap castPtr . V.fromList <$> peekArray maxTextureChannels tptrptr

  vertices <- peekVertexAttributes bm bv ptr

  let ptrs_       = V.fromList [vptr, nptr, tptr] <> tptrs_
      -- [3, 3, 3] stands for the number of components of Vertex/Normal/Tangents
      components_ = V.fromList [3, 3, 3] <> uvs_
      combined_   = V.filter ((/=0) . fst) $ V.zip components_ ptrs_
      (components, ptrs) = (fst <$> combined_, snd <$> combined_)
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

-- | Assumes the mesh in question contains at least one vertex.
marshalAssImpMesh :: A.ScenePtr -> A.MeshPtr -> IO AssImpMesh
marshalAssImpMesh sc ptr = do
  (boneIDMap, boneVector') <- loadBonesFromMesh ptr
  (_, _, faceptr, faceNum, uvs, texOffsets, vertices) <-
    massageAssImpMesh boneIDMap boneVector' ptr

  let texData = V.zip uvs . fmap ((* sizeOf (0 :: CFloat)) . fromIntegral) $ texOffsets
      flags   = defaultBufferAttribFlags
      buffInit size_ ptr_ = initBufferName size_ flags (castPtr ptr_)
      stride = dynamicSizeOfAssImpVertex (vertices V.! 0)
      --stride = sizeOf (0 :: CFloat) * fromIntegral chunkLen

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
  vptr <- interleaveWith dynamicPokeAssImpVertex dynamicSizeOfAssImpVertex vertices
  vbuf <- buffInit (fromIntegral $ length vertices * stride) vptr
  ibuf <- buffInit (fromIntegral $ fromIntegral faceNum * sizeOf (0 :: CUInt)) faceptr

  vertexArrayVertexBuffer vao 0 vbuf 0 (fromIntegral stride)
  free vptr
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
