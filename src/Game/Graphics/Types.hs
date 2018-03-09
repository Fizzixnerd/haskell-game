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
  { _vtnIndexV :: !Int
  , _vtnIndexT :: !Int
  , _vtnIndexN :: !Int
  } deriving (Eq, Show, Ord, Read)

instance Storable VTNIndex where
  sizeOf _ = 3 * sizeOf (0 :: Int)
  alignment _ = alignment (0 :: Int)
  poke ptr (VTNIndex v t n) = do
    pokeByteOff ptr 0 v
    pokeByteOff ptr (1 * sizeOf (0 :: Int)) t
    pokeByteOff ptr (2 * sizeOf (0 :: Int)) n
  peek ptr = do
    v <- peekByteOff ptr 0
    t <- peekByteOff ptr (1 * sizeOf (0 :: Int))
    n <- peekByteOff ptr (2 * sizeOf (0 :: Int))
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

-- * AssImp loading.
data AssImpVertex = AssImpVertex
  { _assImpVertexV :: !(L.V3 Float)
  , _assImpVertexT :: !(L.V2 Float)
  , _assImpVertexN :: !(L.V3 Float)
  } deriving (Eq, Show, Ord, Read)

instance Storable AssImpVertex where
  sizeOf _ = 8 * sizeOf (0 :: Float)
  alignment _ = alignment (0 :: Float)
  poke ptr (AssImpVertex v t n) = do
    pokeByteOff ptr 0 v
    pokeByteOff ptr (3 * sizeOf (0 :: Float)) t
    pokeByteOff ptr (5 * sizeOf (0 :: Float)) n
  peek ptr = do
    v <- peekByteOff ptr 0
    t <- peekByteOff ptr (3 * sizeOf (0 :: Float))
    n <- peekByteOff ptr (5 * sizeOf (0 :: Float))
    return $ AssImpVertex v t n

data AssImpMesh = AssImpMesh
  { _assImpMeshVAO            :: VertexArrayObject
  , _assImpMeshBufferName     :: BufferName
  , _assImpMeshTextureDetails :: Vector Word32
  , _assImpMeshIndexBO        :: BufferName
  , _assImpMeshIndexBOType    :: IndexType
  , _assImpMeshIndexNum       :: Word32
  , _assImpMeshTextureBundle  :: TextureBundle FilePath
  , _assImpMeshShaderMaterial :: ShaderMaterial
  } deriving (Eq, Ord, Show)

newtype AssImpScene = AssImpScene
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

emptyTextureBundle :: TextureBundle s
emptyTextureBundle = TextureBundle Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- * Dynamic buffers
data DynamicBufferBundle = DynamicBufferBundle
  { _dynamicBufferBundleShaderMaterialBuffer :: DynamicBuffer ShaderMaterial
  , _dynamicBufferBundleShaderCameraBuffer :: DynamicBuffer ShaderCamera
  , _dynamicBufferBundlePointLightBundleBuffer :: DynamicBuffer PointLightBundle
  } deriving (Eq, Ord, Show)

-- * Camera and view
type VPMatrix = L.M44 Float
type VMatrix  = L.M44 Float
type PMatrix  = L.M44 Float

-- * Uniform blocks, GLSized and GLStorable instances.
data PointLight = PointLight
  { _pointLightPosition :: L.V4 Float
  , _pointLightIntensity :: Float
  } deriving (Eq, Ord, Show)

instance Storable PointLight where
  sizeOf _ = (4 + 4) * sizeOf (0 :: Float)
  alignment _ = 4 * alignment (error "unreachable" :: Float)
  poke ptr (PointLight loc str) = do
    pokeByteOff ptr 0 loc
    pokeByteOff ptr (4 * sizeOf (0 :: Float)) str
  peek ptr = do
    loc <- peekByteOff ptr 0
    str <- peekByteOff ptr (4 * sizeOf (0 :: Float))
    return $ PointLight loc str

instance GLSized PointLight where
  gSize_ _ = sizeOf (error "unreachable" :: PointLight)

instance GLWritable PointLight where
  gPoke_ = poke

data PointLightBundle = PointLightBundle
  { _pointLightBundleLights :: VS.Vector PointLight
  , _pointLightBundleNum :: Int
  }

instance GLSized PointLightBundle where
  gSize_ _ = maxPointLights * gSize (Proxy :: Proxy PointLight) + sizeOf (0 :: Int)

instance GLWritable PointLightBundle where
  gPoke_ ptr (PointLightBundle ls n) = VS.unsafeWith ls $ \lsPtr -> do
    copyBytes ptr (castPtr lsPtr) $ gSize (Proxy :: Proxy PointLight) * min maxPointLights (length ls)
    poke (castPtr (ptr `plusPtr` (maxPointLights * gSize (Proxy :: Proxy PointLight) ))) n

maxPointLights :: Int
maxPointLights = 4

data PointLightBlock = PointLightBlock deriving (Eq, Ord, Show)

instance ForeignWrite () PointLightBlock (DynamicBuffer PointLightBundle) where
  writeR_ _ _ = bindFullDynamicUniformBuffer PointLightBlock 1

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
      m = 16 * sizeOf (error "unreachable" :: Float)

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
  gSize_ _ = 16 * sizeOf (error "unreachable" :: Float)

instance GLWritable ShaderMaterial where
  gPoke_ ptr ShaderMaterial {..} = do
    pokeByteOff ptr (0*m) _shaderMaterialDiffuseColor
    pokeByteOff ptr (4*m) _shaderMaterialAmbientColor
    pokeByteOff ptr (8*m) _shaderMaterialSpecularColor
    pokeByteOff ptr (12*m) _shaderMaterialSpecularStrength
    pokeByteOff ptr (13*m) _shaderMaterialSpecularExponent
    where
      m = sizeOf (error "unreachable" :: Float)

data ShaderMaterialBlock = ShaderMaterialBlock deriving (Eq, Ord, Show)

instance ForeignWrite () ShaderMaterialBlock (DynamicBuffer ShaderMaterial) where
  writeR_ _ _ = bindFullDynamicUniformBuffer ShaderMaterialBlock 2

mconcat <$> mapM makeLenses
  [ ''ExpandObjVTN
  , ''VTNIndex
  , ''VTNPoint
  , ''AssImpVertex
  , ''AssImpScene
  , ''AssImpMesh
  , ''TextureBundle
  , ''PointLight
  , ''PointLightBundle
  , ''DynamicBufferBundle
  ]
