{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Entity where

import ClassyPrelude
import Game.Types
import Game.Entity.Camera
import Graphics.Binding
import Game.Graphics.Shader.Loader
import qualified Linear as L
import Control.Lens
import qualified Sound.OpenAL as AL
import qualified Physics.Bullet as P
import Foreign.C.Types

getWorldMatrix :: MonadIO m => CollisionBody -> m (L.M44 Float)
getWorldMatrix wt = do
  liftIO $ bracket (P.coAllocateWorldTransform $ wt ^. unCollisionBody) P.del $
    \t -> do
      (CFloat i, CFloat j, CFloat k, CFloat r) <- P.getRotation t
      (CFloat x, CFloat y, CFloat z) <-  P.getOrigin t
      return $ L.mkTransformation (L.Quaternion r (L.V3 i j k)) (L.V3 x y z)

getWorldPosition :: MonadIO m => CollisionBody -> m (L.V3 Float)
getWorldPosition wt = do
  liftIO $ bracket (P.coAllocateWorldTransform $ wt ^. unCollisionBody) P.del $
    \t -> do
      (CFloat x, CFloat y, CFloat z) <- P.getOrigin t
      return $ L.V3 x y z

bindGfxTexture :: MonadIO m => GfxTexture -> m ()
bindGfxTexture GfxTexture {..} = traverse_ (uncurry . flip $ texture) _gfxTexture2D

drawGfxWithTransform :: L.M44 Float -> VPMatrix -> Gfx s -> Game s ()
drawGfxWithTransform wrld vpm gfx = do
  bindGfxTexture (gfx ^. gfxTextureBlob)
  forM_ (gfx ^. gfxVaoData) $ \(vao, prog, mode, size) -> do
    clear $ defaultClearBuffer & clearBufferColor .~ True
                                 & clearBufferDepth .~ True
    useProgram prog
    currentVertexArrayObject $= Just vao
    uniform prog UniformMVP (vpm L.!*! wrld)
    drawElements mode (fromIntegral size) UnsignedInt
  mapM_ (drawGfxWithTransform wrld vpm) $ gfx ^. gfxChildren

drawEntity :: VPMatrix -> Entity s -> Game s ()
drawEntity vpm e = case e ^. entityGraphics of
  Nothing -> return ()
  Just gfx -> do
    wrld <- getWorldMatrix $ e ^. entityCollisionBody
    drawGfxWithTransform wrld vpm gfx

playEntity :: Entity s -> Game s ()
playEntity e = case e ^. entitySounds of
  Nothing -> return ()
  Just sfx -> do
    (L.V3 x y z) <- getWorldPosition $ e ^. entityCollisionBody
    forM_ (sfx ^. sfxSources) $ \s -> do
      AL.sourcePosition s $= AL.Vertex3 (CFloat x) (CFloat y) (CFloat z)
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
  vpm <- cameraVP cam
  e' <- scriptEntity e
  playEntity e'
  drawEntity vpm e'
  return e'

setEntityLinearVelocity :: Entity s -> L.V3 Float -> Game s ()
setEntityLinearVelocity e (L.V3 x y z) = case e ^. entityRigidBody of
  Nothing -> return ()
  Just (RigidBody rb) -> liftIO $ P.rbSetLinearVelocity rb (CFloat x) (CFloat y) (CFloat z)

entityLocalClosestRayCast :: Entity s
                          -> L.V3 Float -- ^ in Entity-local coordinates
                          -> (P.CollisionObject -> Game s ()) -- ^ callback
                          -> Game s ()
entityLocalClosestRayCast e v cb = do
  dw <- use $ gameStatePhysicsWorld . physicsWorldDynamicsWorld
  entityCenter@(L.V3 fromx fromy fromz) <- getWorldPosition $ e ^. entityCollisionBody
  let (L.V3 tox toy toz) = entityCenter + v
  crrc :: P.ClosestRayResultCallback <- liftIO $ P.new (((CFloat fromx), (CFloat fromy), (CFloat fromz)),
                          ((CFloat tox), (CFloat toy), (CFloat toz)))
  liftIO $ P.rayTest dw (CFloat fromx) (CFloat fromy) (CFloat fromz)
    (CFloat tox) (CFloat toy) (CFloat toz) crrc
  hasHit <- liftIO $ P.rrcHasHit crrc
  if hasHit
    then cb =<< (liftIO $ P.rrcGetHit crrc)
    else return ()
