{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Entity.Gfx where

import Game.Entity.WorldTransform
import ClassyPrelude
import Control.Lens
import Foreign.Resource
import Game.Entity.Camera
import Game.Graphics.Texture.Loader
import Game.Types
import Graphics.Binding
import qualified Linear as L

bindTextureBundle :: MonadIO m => TextureBundle (TextureObject TextureTarget2D) -> m ()
bindTextureBundle TextureBundle {..} = do
  within _textureBundleDiffuseTexture      Diffuse2DSampler
  within _textureBundleSpecularTexture     Specular2DSampler
  within _textureBundleAmbientTexture      Ambient2DSampler
  within _textureBundleEmmisiveTexture     Emmisive2DSampler
  within _textureBundleHeightTexture       Height2DSampler
  within _textureBundleNormalTexture       Normal2DSampler
  within _textureBundleShininessTexture    Shininess2DSampler
  within _textureBundleOpacityTexture      Opacity2DSampler
  within _textureBundleDisplacementTexture Displacement2DSampler
  within _textureBundleLightMapTexture     LightMap2DSampler
  within _textureBundleReflectionTexture   Reflection2DSampler
  where
    within x y = mapM_ (`texture` y) x

drawGfxWithTransform :: L.M44 Float -> Camera s -> Gfx s -> Game s ()
drawGfxWithTransform wrld cam gfx = do
  -- vpm <- cameraVP cam
  camV <- cam ^. to cameraV
  camVP <- cam ^. to cameraVP
  let shaderCam = ShaderCamera
                  { _shaderCameraP = cam ^. to cameraP
                  , _shaderCameraMV = camV L.!*! wrld
                  , _shaderCameraMVP = camVP L.!*! wrld
                  }
  s1dt <- use gameStateSpecular1DTexture
  texture s1dt SpecularToon1DSampler;
  forM_ (gfx ^. gfxVaoData) $ \VaoData {..} -> do
    bindTextureBundle _vaoDataTextureBundle
    ActivePipeline $= Just _vaoDataShaderPipeline
    ActiveVertexArrayObject $= Just _vaoDataVao

    scdb <- use $ gameStateDynamicBufferBundle . dynamicBufferBundleShaderCameraBuffer
    scdb ~& FullBufferWrite .$= shaderCam

    smdb <- use $ gameStateDynamicBufferBundle . dynamicBufferBundleShaderMaterialBuffer
    smdb ~& FullBufferWrite .$= _vaoDataShaderMaterial

    drawElements _vaoDataPrimitiveMode (fromIntegral _vaoDataNumElements) UnsignedInt

  mapM_ (drawGfxWithTransform wrld cam) $ gfx ^. gfxChildren

drawEntity :: Camera s -> Entity s -> Game s ()
drawEntity cam e = case e ^. entityGraphics of
  Nothing -> return ()
  Just gfx -> do
    wrld <- getWorldMatrix $ e ^. entityCollisionObject
    drawGfxWithTransform wrld cam gfx
