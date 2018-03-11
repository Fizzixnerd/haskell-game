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
  ]
