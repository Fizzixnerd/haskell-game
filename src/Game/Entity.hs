{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Entity where

import ClassyPrelude
import Game.Types
import Game.Entity.Camera
import Graphics.Binding
-- import Game.Graphics.Shader.Loader
import Game.Graphics.Texture.Loader
import qualified Linear as L
import Control.Lens
import qualified Sound.OpenAL as AL
import qualified Physics.Bullet as P
import Foreign.C.Types
import Foreign.Resource

getWorldMatrix :: MonadIO m => CollisionObject -> m (L.M44 Float)
getWorldMatrix wt =
  liftIO $ bracket (P.coAllocateWorldTransform $ wt ^. unCollisionObject) P.del $
    \t -> do
      (CFloat i, CFloat j, CFloat k, CFloat r) <- P.getRotation t
      (CFloat x, CFloat y, CFloat z) <-  P.getOrigin t
      return $ L.mkTransformation (L.Quaternion r (L.V3 i j k)) (L.V3 x y z)

getWorldPosition :: MonadIO m => CollisionObject -> m (L.V3 Float)
getWorldPosition wt =
  liftIO $ bracket (P.coAllocateWorldTransform $ wt ^. unCollisionObject) P.del $
    \t -> do
      (CFloat x, CFloat y, CFloat z) <- P.getOrigin t
      return $ L.V3 x y z

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

playEntity :: Entity s -> Game s ()
playEntity e = case e ^. entitySounds of
  Nothing -> return ()
  Just sfx -> do
    (L.V3 x y z) <- getWorldPosition $ e ^. entityCollisionObject
    forM_ (sfx ^. sfxSources) $ \s -> do
      AL.sourcePosition s AL.$= AL.Vertex3 (CFloat x) (CFloat y) (CFloat z)
      sState <- liftIO $ AL.sourceState s
      case sState of
        AL.Initial -> AL.play [s]
        _ -> return ()

scriptEntity :: Entity s -> Game s (Entity s)
scriptEntity e = case e ^. entityLogic of
  Nothing -> return e
  Just lfx -> foldlM (\ent scr -> scr ent) e (lfx ^. lfxScripts)

animateEntity :: Entity s -> Game s (Entity s)
animateEntity e = do
  cam <- use gameStateCamera
  e' <- scriptEntity e
  playEntity e'
  drawEntity cam e'
  return e'

entityLocalClosestRayCast :: Entity s
                          -> L.V3 Float -- ^ in Entity-local coordinates
                          -> (Entity s -> Game s ()) -- ^ callback
                          -> Game s ()
entityLocalClosestRayCast e v cb = do
  dw <- use $ gameStatePhysicsWorld . physicsWorldDynamicsWorld
  entityCenter@(L.V3 fromx fromy fromz) <- getWorldPosition $ e ^. entityCollisionObject
  let (L.V3 tox toy toz) = entityCenter + v
  crrc :: P.ClosestRayResultCallback <- liftIO $ P.new ((CFloat fromx, CFloat fromy, CFloat fromz),
                                                        (CFloat tox, CFloat toy, CFloat toz))
  liftIO $ P.rayTest dw (CFloat fromx) (CFloat fromy) (CFloat fromz)
    (CFloat tox) (CFloat toy) (CFloat toz) crrc
  whenM (liftIO . P.rrcHasHit $ crrc) $ do
    co <- liftIO $ P.rrcGetHit crrc
    n <- liftIO $ P.getUserIndex co
    es <- use $ gameStatePhysicsWorld . physicsWorldEntities
    cb $ es `unsafeIndex` n

entityApplyForce :: MonadIO m => Entity s -> L.V3 Float -> m ()
entityApplyForce e (L.V3 x y z) = liftIO $
  case e ^. entityRigidBody of
    Just (RigidBody rb) -> P.applyForce rb (CFloat x) (CFloat y) (CFloat z) 0 0 0
    Nothing -> return ()

entityApplyTorque :: MonadIO m => Entity s -> L.V3 Float -> m ()
entityApplyTorque e (L.V3 x y z) = liftIO $
  case e ^. entityRigidBody of
    Just (RigidBody rb) -> P.applyTorque rb (CFloat x) (CFloat y) (CFloat z)
    Nothing -> return ()

allocateEntityTransform :: MonadIO m => Entity s -> m P.Transform
allocateEntityTransform e = liftIO $ P.coAllocateWorldTransform $ e ^. entityCollisionObject . unCollisionObject

withEntityTransform :: MonadIO m => Entity s -> (P.Transform -> IO b) -> m b
withEntityTransform e f = liftIO $ bracket (allocateEntityTransform e) P.del f

getEntityOrientation e = liftIO $ do
  (CFloat i, CFloat j, CFloat k, CFloat r) <- withEntityTransform e P.getRotation
  return $ L.Quaternion r (L.V3 i j k)

setEntityOrientation :: MonadIO m => Entity s -> L.Quaternion Float -> m ()
setEntityOrientation e (L.Quaternion r (L.V3 i j k)) = liftIO $ do
  withEntityTransform e $ \t -> do
    P.setRotation t (CFloat i) (CFloat j) (CFloat k) (CFloat r)
    P.coSetWorldTransform (e ^. entityCollisionObject . unCollisionObject) t

getEntityLinearVelocity :: MonadIO m => Entity s -> m (L.V3 Float)
getEntityLinearVelocity e = liftIO $
  case e ^. entityRigidBody of
    Nothing -> do
      (CFloat x, CFloat y, CFloat z) <- P.getInterpolationLinearVelocity $
                                        e ^. entityCollisionObject . unCollisionObject
      return $ L.V3 x y z
    Just (RigidBody rb) -> do
      (CFloat x, CFloat y, CFloat z) <- P.rbGetLinearVelocity rb
      return $ L.V3 x y z

setEntityLinearVelocity :: MonadIO m => Entity s -> L.V3 Float -> m ()
setEntityLinearVelocity e (L.V3 x y z) = liftIO $
  case e ^. entityRigidBody of
    Nothing -> P.setInterpolationLinearVelocity
               (e ^. entityCollisionObject . unCollisionObject)
               (CFloat x) (CFloat y) (CFloat z)
    Just (RigidBody rb) -> P.rbSetLinearVelocity rb (CFloat x) (CFloat y) (CFloat z)

getEntityAngularVelocity :: MonadIO m => Entity s -> m (L.V3 Float)
getEntityAngularVelocity e = liftIO $
  case e ^. entityRigidBody of
    Nothing -> do
      (CFloat x, CFloat y, CFloat z) <- P.getInterpolationAngularVelocity $
                                        e ^. entityCollisionObject . unCollisionObject
      return $ L.V3 x y z
    Just (RigidBody rb) -> do
      (CFloat x, CFloat y, CFloat z) <- P.rbGetAngularVelocity rb
      return $ L.V3 x y z

setEntityAngularVelocity :: MonadIO m => Entity s -> L.V3 Float -> m ()
setEntityAngularVelocity e (L.V3 x y z) = liftIO $
  case e ^. entityRigidBody of
    Nothing -> P.setInterpolationAngularVelocity
               (e ^. entityCollisionObject . unCollisionObject)
               (CFloat x) (CFloat y) (CFloat z)
    Just (RigidBody rb) -> P.rbSetAngularVelocity rb (CFloat x) (CFloat y) (CFloat z)
