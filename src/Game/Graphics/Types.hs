{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Graphics.Types where

import qualified Asset.AssImp.Types as A
import ClassyPrelude
import Foreign.Resource
import Graphics.Binding
import qualified Linear as L
import Foreign.C.Types
import Foreign.Storable
import qualified Data.Map.Strict as MS
import qualified Data.Vector.Storable as VS
import qualified Codec.Wavefront as W
import Data.Typeable
import Foreign.Ptr
import Foreign.Marshal.Utils
import Control.Lens
import qualified Data.Vector as V
import qualified Prelude

-- * Obj file loading.
data VTNPoint = VTNPoint
  { _vtnPointV :: !(L.V4 CFloat)
  , _vtnPointT :: !(L.V2 CFloat)
  , _vtnPointN :: !(L.V3 CFloat)
  } deriving (Eq, Show, Ord, Read)

instance Storable VTNPoint where
  sizeOf _ = 9 * sizeOf (0 :: CFloat)
  alignment _ = alignment (0 :: CFloat)
  poke ptr (VTNPoint v t n) = do
    pokeByteOff ptr 0 v
    pokeByteOff ptr (4 * sizeOf (0 :: CFloat)) t
    pokeByteOff ptr (6 * sizeOf (0 :: CFloat)) n
  peek ptr = do
    v <- peekByteOff ptr 0
    t <- peekByteOff ptr (4 * sizeOf (0 :: CFloat))
    n <- peekByteOff ptr (6 * sizeOf (0 :: CFloat))
    return $ VTNPoint v t n

data VTNIndex = VTNIndex
  { _vtnIndexV :: !Int32
  , _vtnIndexT :: !Int32
  , _vtnIndexN :: !Int32
  } deriving (Eq, Show, Ord, Read)

instance Storable VTNIndex where
  sizeOf _ = 3 * sizeOf (0 :: Int32)
  alignment _ = alignment (0 :: Int32)
  poke ptr (VTNIndex v t n) = do
    pokeByteOff ptr 0 v
    pokeByteOff ptr (1 * sizeOf (0 :: Int32)) t
    pokeByteOff ptr (2 * sizeOf (0 :: Int32)) n
  peek ptr = do
    v <- peekByteOff ptr 0
    t <- peekByteOff ptr (1 * sizeOf (0 :: Int32))
    n <- peekByteOff ptr (2 * sizeOf (0 :: Int32))
    return $ VTNIndex v t n

data ExpandObjVTN = ExpandObjVTN
  { _expandObjVTNIndMap :: MS.Map VTNIndex CUInt
  , _expandObjVTNPoints :: [VTNPoint]
  , _expandObjVTNIndices :: [CUInt]
  , _expandObjVTNNextInd :: CUInt
  , _expandObjVTNVerts :: Vector W.Location
  , _expandObjVTNTexs :: Vector W.TexCoord
  , _expandObjVTNNorms :: Vector W.Normal
  } deriving (Eq, Show)

type BoneWeight = Float

-- * AssImp loading.
data AssImpVertex = AssImpVertex
  { _assImpVertexV     :: !(L.V3 Float)
  , _assImpVertexN     :: !(L.V3 Float)
  , _assImpVertexT     :: !(L.V3 Float)
  , _assImpBoneIDs     :: !(L.V4 BoneID)
  , _assImpBoneWeights :: !(L.V4 BoneWeight)
  , _assImpTex2D0      :: !(Maybe (L.V2 Float))
  , _assImpTex2D1      :: !(Maybe (L.V2 Float))
  , _assImpTex2D2      :: !(Maybe (L.V2 Float))
  , _assImpTex2D3      :: !(Maybe (L.V2 Float))
  , _assImpTex3D0      :: !(Maybe (L.V3 Float))
  , _assImpTex3D1      :: !(Maybe (L.V3 Float))
  , _assImpTex1D0      :: !(Maybe Float)
  , _assImpTex1D1      :: !(Maybe Float)
  } deriving (Eq, Show, Ord, Read)

dynamicSizeOfAssImpVertex :: AssImpVertex -> Int
dynamicSizeOfAssImpVertex (AssImpVertex _ _ _ _ _ uv0 uv1 uv2 uv3 uvw0 uvw1 u0 u1) =
  13 * sizeOf (0 :: Float) + 4 * sizeOf (0 :: Int) +
  sizeOf2 uv0 +
  sizeOf2 uv1 +
  sizeOf2 uv2 +
  sizeOf2 uv3 +
  sizeOf3 uvw0 +
  sizeOf3 uvw1 +
  sizeOf1 u0 +
  sizeOf1 u1
  where
    sizeOf2 :: Maybe (L.V2 Float) -> Int
    sizeOf2 = maybe 0 (const $ 2 * sizeOf (0 :: Float))

    sizeOf1 :: Maybe Float -> Int
    sizeOf1 = maybe 0 (const $ 1 * sizeOf (0 :: Float))

    sizeOf3 :: Maybe (L.V3 Float) -> Int
    sizeOf3 = maybe 0 (const $ 3 * sizeOf (0 :: Float))

dynamicAlignmentAssImpVertex :: AssImpVertex -> Int
dynamicAlignmentAssImpVertex _ = Prelude.lcm (alignment (0 :: Float)) (alignment (0 :: Int32))

dynamicPokeAssImpVertex :: Ptr AssImpVertex -> AssImpVertex -> IO ()
dynamicPokeAssImpVertex ptr (AssImpVertex v t n bids bweights uv0 uv1 uv2 uv3 uvw0 uvw1 u0 u1) = do
  pokeByteOff ptr 0 v
  pokeByteOff ptr (3 * sizeOf (0 :: Float)) n
  pokeByteOff ptr (6 * sizeOf (0 :: Float)) t
  -- This is Float and that's correct. The Int bit goes in the NEXT offset.
  pokeByteOff ptr (9 * sizeOf (0 :: Float)) bids
  pokeByteOff ptr (9 * sizeOf (0 :: Float) + 4 * sizeOf (0 :: Int32)) bweights
  -- Next one will be 13 * Float + 4 * Int.
  let offset0 = 13 * sizeOf (0 :: Float) + 4 * sizeOf (0 :: Int32)
  offset1 <- case uv0 of
    Nothing -> return offset0
    Just uv0' -> do
      pokeByteOff ptr offset0 uv0'
      return $ offset0 + 2 * sizeOf (0 :: Float)
  offset2 <- case uv1 of
    Nothing -> return offset1
    Just uv1' -> do
      pokeByteOff ptr offset1 uv1'
      return $ offset1 + 2 * sizeOf (0 :: Float)
  offset3 <- case uv2 of
    Nothing -> return offset2
    Just uv2' -> do
      pokeByteOff ptr offset2 uv2'
      return $ offset2 + 2 * sizeOf (0 :: Float)
  offset4 <- case uv3 of
    Nothing -> return offset3
    Just uv3' -> do
      pokeByteOff ptr offset3 uv3'
      return $ offset3 + 2 * sizeOf (0 :: Float)
  offset5 <- case uvw0 of
    Nothing -> return offset4
    Just uvw0' -> do
      pokeByteOff ptr offset4 uvw0'
      return $ offset4 + 3 * sizeOf (0 :: Float)
  offset6 <- case uvw1 of
    Nothing -> return offset5
    Just uvw1' -> do
      pokeByteOff ptr offset5 uvw1'
      return $ offset5 + 3 * sizeOf (0 :: Float)
  offset7 <- case u0 of
    Nothing -> return offset6
    Just u0' -> do
      pokeByteOff ptr offset6 u0'
      return $ offset6 + sizeOf (0 :: Float)
  forM_ u1 (pokeByteOff ptr offset7)

type BoneMap = Map BoneName Bone
type BoneIDMap = Map BoneName BoneID
type BoneAnimationMap = Map AnimationName BoneAnimation
type AnimationID = Int32
type AnimationIDMap = Map AnimationName AnimationID

data AssImpMesh = AssImpMesh
  { _assImpMeshVAO            :: VertexArrayObject
  , _assImpMeshBufferName     :: BufferName
  , _assImpMeshIndexBO        :: BufferName
  , _assImpMeshIndexBOType    :: IndexType
  , _assImpMeshIndexNum       :: Word32
  , _assImpMeshTextureBundle  :: TextureBundle FilePath
  , _assImpMeshShaderMaterial :: ShaderMaterial
  , _assImpMeshBones          :: Vector (Bone, Vector BoneAnimation)
  , _assImpMeshBoneIDMap      :: BoneIDMap
  , _assImpMeshAnimationIDMap :: AnimationIDMap
  } deriving (Eq, Ord, Show)

data AssImpScene = AssImpScene
  { _assImpMeshes :: Vector AssImpMesh
  } deriving (Eq, Ord, Show)

-- * Texture data
data TextureBundle s = TextureBundle
  { _textureBundleDiffuseTexture      :: Maybe s
  , _textureBundleSpecularTexture     :: Maybe s
  , _textureBundleAmbientTexture      :: Maybe s
  , _textureBundleEmmisiveTexture     :: Maybe s
  , _textureBundleHeightTexture       :: Maybe s
  , _textureBundleNormalTexture       :: Maybe s
  , _textureBundleShininessTexture    :: Maybe s
  , _textureBundleOpacityTexture      :: Maybe s
  , _textureBundleDisplacementTexture :: Maybe s
  , _textureBundleLightMapTexture     :: Maybe s
  , _textureBundleReflectionTexture   :: Maybe s
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- * Animation data
type BoneID = Int32
type NodeID = Int32
type VertexID = Int32
type Weight = Float

type BoneName = Text

data Bone = Bone
  { _boneName :: BoneName
  , _boneID :: BoneID
  , _boneWeights :: Vector (VertexID, Weight)
  , _boneMatrix :: L.M44 Float
  } deriving (Eq, Ord, Show)

type AnimationName = Text

data Animation = Animation
                 { _animationName :: AnimationName
                 , _animationDuration :: Double
                 , _animationTicksPerSecond :: Double
                 , _animationChannels :: Vector NodeAnim
                 , _animationMeshChannels :: Vector MeshAnim
                 , _animationMeshMorphChannels :: Vector MeshMorphAnim
                 } deriving (Eq, Ord, Show)

peekAnimation :: A.AnimationPtr -> IO Animation
peekAnimation aptr = do
  name <- fromString <$> A.peekAIString (A.animationName aptr)
  (CDouble duration) <- A.animationDuration aptr
  (CDouble tps) <- A.animationTicksPerSecond aptr
  numChans <- fromIntegral <$> A.animationNumChannels aptr
  chansPtr <- A.animationChannels aptr
  chans <- V.generateM numChans $ \i ->
    peekNodeAnim $ chansPtr `plusPtr` (i * A.sizeOfNodeAnim)
  numMeshChans <- fromIntegral <$> A.animationNumMeshChannels aptr
  meshChansPtr <- A.animationMeshChannels aptr
  meshChans <- V.generateM numMeshChans $ \i ->
    peekMeshAnim $ meshChansPtr `plusPtr` (i * A.sizeOfMeshAnim)
  numMeshMorphChans <- fromIntegral <$> A.animationNumMorphMeshChannels aptr
  meshMorphChansPtr <- A.animationMorphMeshChannels aptr
  meshMorphChans <- V.generateM numMeshMorphChans $ \i ->
    peekMeshMorphAnim $ meshMorphChansPtr `plusPtr` (i * A.sizeOfMeshMorphAnim)
  return $ Animation name duration tps chans meshChans meshMorphChans

data NodeAnim = NodeAnim
                { _nodeAnimName :: BoneName
                , _nodeAnimPositionKeys :: Vector PositionKey
                , _nodeAnimRotationKeys :: Vector RotationKey
                , _nodeAnimScalingKeys  :: Vector ScalingKey
                , _nodeAnimPreState     :: A.AnimBehaviour
                , _nodeAnimPostState    :: A.AnimBehaviour
                } deriving (Eq, Ord, Show)

peekNodeAnim :: A.NodeAnimPtr -> IO NodeAnim
peekNodeAnim anp = do
  name <- fromString <$> A.peekAIString (A.nodeAnimNodeName anp)
  numPosKeys <- fromIntegral <$> A.nodeAnimNumPositionKeys anp
  posKeysPtr <- A.nodeAnimPositionKeys anp
  posKeys <- V.generateM numPosKeys $ \i ->
    peekPositionKey $ posKeysPtr `plusPtr` (i * A.sizeOfVectorKey)
  numRotKeys <-fromIntegral <$> A.nodeAnimNumRotationKeys anp
  rotKeysPtr <- A.nodeAnimRotationKeys anp
  rotKeys <- V.generateM numRotKeys $ \i ->
    peekRotationKey $ rotKeysPtr `plusPtr` (i * A.sizeOfQuatKey)
  numScaKeys <- fromIntegral <$> A.nodeAnimNumScalingKeys anp
  scaKeysPtr <- A.nodeAnimScalingKeys anp
  scaKeys <- V.generateM numScaKeys $ \i ->
    peekScalingKey $ scaKeysPtr `plusPtr` (i * A.sizeOfVectorKey)
  preState <- A.nodeAnimPreState anp
  postState <- A.nodeAnimPostState anp
  return $ NodeAnim name posKeys rotKeys scaKeys preState postState

data BoneAnimation = BoneAnimation
                     { _boneAnimationName :: AnimationName
                     , _boneAnimationPositions :: Vector PositionKey
                     , _boneAnimationRotations :: Vector RotationKey
                     , _boneAnimationScalings  :: Vector ScalingKey
                     , _boneAnimationDuration :: Double
                     , _boneAnimationTicksPerSecond :: Double
                     } deriving (Eq, Ord, Show)

-- TODO: Implement these!
data MeshAnim = MeshAnim deriving (Eq, Ord, Show)

peekMeshAnim :: A.MeshAnimPtr -> IO MeshAnim
peekMeshAnim = const $ return MeshAnim

data MeshMorphAnim = MeshMorphAnim deriving (Eq, Ord, Show)

peekMeshMorphAnim :: A.MeshMorphAnimPtr -> IO MeshMorphAnim
peekMeshMorphAnim = const $ return MeshMorphAnim

data PositionKey = PositionKey
                   { _positionKeyTime :: Double
                   , _positionKeyPosition :: L.V3 Float
                   } deriving (Eq, Ord, Show)

peekPositionKey :: A.VectorKeyPtr -> IO PositionKey
peekPositionKey vkp = do
  (CDouble time) <- A.vectorKeyTime vkp
  pos <- A.peekVector3D $ A.vectorKeyValue vkp
  return $ PositionKey time pos

data RotationKey = RotationKey
                   { _rotationKeyTime :: Double
                   , _rotationKeyRotation :: L.Quaternion Float
                   } deriving (Eq, Ord, Show)

peekRotationKey :: A.QuatKeyPtr -> IO RotationKey
peekRotationKey qkp = do
  (CDouble time) <- A.quatKeyTime qkp
  rot <- A.peekQuaternion $ A.quatKeyValue qkp
  return $ RotationKey time rot

data ScalingKey = ScalingKey
                  { _scalingKeyTime :: Double
                  , _scalingKeyScaling :: L.V3 Float
                  } deriving (Eq, Ord, Show)

peekScalingKey :: A.VectorKeyPtr -> IO ScalingKey
peekScalingKey vkp = do
  (CDouble time) <- A.vectorKeyTime vkp
  sca <- A.peekVector3D $ A.vectorKeyValue vkp
  return $ ScalingKey time sca

data Node = Node
  { _nodeName :: Text
  , _nodeID :: NodeID
  , _nodeMatrix :: L.M44 Float
  , _nodeParent :: Maybe NodeID
  , _nodeChildren :: Vector NodeID
  } deriving (Eq, Ord, Show)

data NodeBundle = NodeBundle
  { _nodeBundleNodes :: Vector (Node, Maybe (Bone, Vector BoneAnimation))
  , _nodeBundleBoneIDMap :: Map BoneName BoneID
  } deriving (Eq, Ord, Show)

emptyTextureBundle :: TextureBundle s
emptyTextureBundle = TextureBundle Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- * Dynamic buffers
data DynamicBufferBundle = DynamicBufferBundle
  { _dynamicBufferBundleShaderMaterialBuffer :: DynamicBuffer ShaderMaterial
  , _dynamicBufferBundleShaderCameraBuffer :: DynamicBuffer ShaderCamera
  , _dynamicBufferBundleLightBundleBuffer :: DynamicBuffer LightBundle
  } deriving (Eq, Ord, Show)

-- * Camera and view
type VPMatrix = L.M44 Float
type VMatrix  = L.M44 Float
type PMatrix  = L.M44 Float

-- * Uniform blocks, GLSized and GLStorable instances.
data Light = Light
  { _lightPosition :: L.V4 Float
  , _lightIntensity :: Float
  } deriving (Eq, Ord, Show)

instance Storable Light where
  sizeOf _ = (4 + 4) * sizeOf (0 :: Float)
  alignment _ = 4 * alignment (error "unreachable" :: Float)
  poke ptr (Light loc str) = do
    pokeByteOff ptr 0 loc
    pokeByteOff ptr (4 * sizeOf (0 :: Float)) str
  peek ptr = do
    loc <- peekByteOff ptr 0
    str <- peekByteOff ptr (4 * sizeOf (0 :: Float))
    return $ Light loc str

instance GLSized Light where
  gSize_ _ = sizeOf (error "unreachable" :: Light)

instance GLWritable Light where
  gPoke_ = poke

data LightBundle = LightBundle
  { _lightBundleLights :: VS.Vector Light
  , _lightBundleNum :: Int
  }

maxLights :: Int
maxLights = 4

instance GLSized LightBundle where
  gSize_ _ = maxLights * gSize (Proxy :: Proxy Light) + sizeOf (0 :: Int)

instance GLWritable LightBundle where
  gPoke_ ptr (LightBundle ls n) = VS.unsafeWith ls $ \lsPtr -> do
    copyBytes ptr (castPtr lsPtr) $ gSize (Proxy :: Proxy Light) * min maxLights (length ls)
    poke (castPtr (ptr `plusPtr` (maxLights * gSize (Proxy :: Proxy Light) ))) n

data LightBlock = LightBlock deriving (Eq, Ord, Show)

instance ForeignWrite () LightBlock (DynamicBuffer LightBundle) where
  writeR_ _ _ = bindFullDynamicUniformBuffer LightBlock 1

data ShaderCamera = ShaderCamera
  { _shaderCameraMVP :: L.M44 Float
  , _shaderCameraMV  :: L.M44 Float
  , _shaderCameraP   :: L.M44 Float
  } deriving (Eq, Ord, Show)

instance GLSized ShaderCamera where
  gSize_ _ = 3 * sizeOf (error "unreachable" :: L.M44 Float)

instance GLWritable ShaderCamera where
  gPoke_ ptr (ShaderCamera mvp vp p) = do
    pokeByteOff ptr (0*m) mvp
    pokeByteOff ptr (1*m) vp
    pokeByteOff ptr (2*m) p
    where
      m = 16 * sizeOf (0 :: Float)

data CameraBlock = CameraBlock deriving (Eq, Ord, Show)

instance ForeignWrite () CameraBlock (DynamicBuffer ShaderCamera) where
  writeR_ _ _ = bindFullDynamicUniformBuffer CameraBlock 0

data ShaderMaterial = ShaderMaterial
  { _shaderMaterialDiffuseColor     :: L.V4 Float
  , _shaderMaterialAmbientColor     :: L.V4 Float
  , _shaderMaterialSpecularColor    :: L.V4 Float
  , _shaderMaterialSpecularStrength :: Float
  , _shaderMaterialSpecularExponent :: Float
  } deriving (Eq, Ord, Show)

instance GLSized ShaderMaterial where
  gSize_ _ = 16 * sizeOf (0 :: Float)

instance GLWritable ShaderMaterial where
  gPoke_ ptr ShaderMaterial {..} = do
    pokeByteOff ptr (0*m) _shaderMaterialDiffuseColor
    pokeByteOff ptr (4*m) _shaderMaterialAmbientColor
    pokeByteOff ptr (8*m) _shaderMaterialSpecularColor
    pokeByteOff ptr (12*m) _shaderMaterialSpecularStrength
    pokeByteOff ptr (13*m) _shaderMaterialSpecularExponent
    where
      m = sizeOf (0 :: Float)

data ShaderMaterialBlock = ShaderMaterialBlock deriving (Eq, Ord, Show)

instance ForeignWrite () ShaderMaterialBlock (DynamicBuffer ShaderMaterial) where
  writeR_ _ _ = bindFullDynamicUniformBuffer ShaderMaterialBlock 2

-- * Compressed point light.

data LightCompressed = LightCompressed
  { _lightCompressedLightPosition  :: L.V3 Float
  , _lightCompressedLightIntensity :: Float
  } deriving (Eq, Ord, Show)

instance Storable LightCompressed where
  sizeOf _   = 4 * sizeOf (error "mu" :: Float)
  alignment _ = alignment (error "mu" :: Float)
  poke ptr (LightCompressed (L.V3 x y z) i) = do
    pokeByteOff ptr (0*m) x
    pokeByteOff ptr (1*m) y
    pokeByteOff ptr (2*m) z
    pokeByteOff ptr (3*m) i
    where
      m = sizeOf (error "mu" :: Float)
  peek ptr  = do
    x <- peekByteOff ptr $ 0 * m
    y <- peekByteOff ptr $ 1 * m
    z <- peekByteOff ptr $ 2 * m
    i <- peekByteOff ptr $ 3 * m
    return $ LightCompressed (L.V3 x y z) i
    where
      m = sizeOf (error "mu" :: Float)

instance GLSized LightCompressed where
  gSize_ _ = sizeOf (error "mu" :: LightCompressed)

instance GLWritable LightCompressed where
  gPoke_ = poke

-- * Point light

newtype PointLight = PointLight
  { _pointLightLightCompressed :: LightCompressed
  } deriving (Eq, Ord, Show, Storable, GLSized, GLWritable)

-- * Directional light

data DirectionalLight = DirectionalLight
  { _directionalLightLightPosition  :: L.V3 Float
  , _directionalLightLightIntensity :: Float
  } deriving (Eq, Ord, Show)

instance Storable DirectionalLight where
  sizeOf _   = 4 * sizeOf (error "mu" :: Float)
  alignment _ = alignment (error "mu" :: Float)
  poke ptr (DirectionalLight (L.V3 x y z) i) = do
    pokeByteOff ptr (0*m) x
    pokeByteOff ptr (1*m) y
    pokeByteOff ptr (2*m) z
    pokeByteOff ptr (3*m) i
    where
      m = sizeOf (error "mu" :: Float)
  peek ptr  = do
    x <- peekByteOff ptr $ 0 * m
    y <- peekByteOff ptr $ 1 * m
    z <- peekByteOff ptr $ 2 * m
    i <- peekByteOff ptr $ 3 * m
    return $ DirectionalLight (L.V3 x y z) i
    where
      m = sizeOf (error "mu" :: Float)

instance GLSized DirectionalLight where
  gSize_ _ = sizeOf (error "mu" :: DirectionalLight)

instance GLWritable DirectionalLight where
  gPoke_ = poke

data DirectionalLightBundle = DirectionalLightBundle
  { _directionalBundleLights :: VS.Vector DirectionalLight
  , _directionalBundleNum :: Int
  } deriving (Eq, Ord, Show)

maxDirectionalLights :: Int
maxDirectionalLights = 4

instance GLSized DirectionalLightBundle where
  gSize_ _ = maxDirectionalLights * gSize (Proxy :: Proxy DirectionalLight) + sizeOf (0 :: Int)

instance GLWritable DirectionalLightBundle where
  gPoke_ ptr (DirectionalLightBundle ls n) = VS.unsafeWith ls $ \lsPtr -> do
    copyBytes ptr (castPtr lsPtr) $ gSize (Proxy :: Proxy Light) * min maxLights (length ls)
    poke (castPtr (ptr `plusPtr` (maxLights * gSize (Proxy :: Proxy Light) ))) n

data DirectionalLightBlock = DirectionalLightBlock deriving (Eq, Ord, Show)

instance ForeignWrite () DirectionalLightBlock (DynamicBuffer DirectionalLightBundle) where
  writeR_ _ _ = bindFullDynamicUniformBuffer DirectionalLightBlock (error "Set me to a particular binding point!")

-- * Text buffering
newtype TextBuffer = TextBuffer
  { _textBufferBufferText :: Text
  } deriving (Eq, Ord, Show)

initTextBuffer :: TextBuffer
initTextBuffer = TextBuffer ""

data TextBufferUpdate
  = TextBufferAdd Char
  | TextBufferBackSpace

newtype DevConsole = DevConsole
  { _devConsoleTextBuffer :: TextBuffer
  } deriving (Eq, Ord, Show)

initDevConsole :: DevConsole
initDevConsole = DevConsole initTextBuffer

makeFields ''LightCompressed
makeFields ''Light
makeFields ''PointLight
makeFields ''DirectionalLight
makeFields ''TextBuffer
makeFields ''DevConsole

instance HasLightPosition PointLight (L.V3 Float) where
  lightPosition = lightCompressed . lightPosition

instance HasLightIntensity PointLight Float where
  lightIntensity = lightCompressed . lightIntensity

instance HasBufferText DevConsole Text where
  bufferText = textBuffer . bufferText

mconcat <$> mapM makeLenses
  [ ''ExpandObjVTN
  , ''VTNIndex
  , ''VTNPoint
  , ''AssImpVertex
  , ''AssImpScene
  , ''AssImpMesh
  , ''TextureBundle
  , ''LightBundle
  , ''DynamicBufferBundle
  , ''Bone
  , ''Node
  , ''NodeBundle
  , ''NodeAnim
  , ''Animation
  ]
