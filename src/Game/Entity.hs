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

toWorldMatrix :: MonadIO m => WorldTransform -> m (L.M44 Float)
toWorldMatrix wt = do
  liftIO $ bracket (P.coAllocateWorldTransform $ wt ^. unWorldTransform) P.del $
    \t -> do
      (CFloat i, CFloat j, CFloat k, CFloat r) <- P.getRotation t
      (CFloat x, CFloat y, CFloat z) <-  P.getOrigin t
      return $ L.mkTransformation (L.Quaternion r (L.V3 i j k)) (L.V3 x y z)

toWorldPosition :: MonadIO m => WorldTransform -> m (L.V3 Float)
toWorldPosition wt = do
  liftIO $ bracket (P.coAllocateWorldTransform $ wt ^. unWorldTransform) P.del $
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
    wrld <- toWorldMatrix $ e ^. entityWorldTransform
    drawGfxWithTransform wrld vpm gfx

playEntity :: Entity s -> Game s ()
playEntity e = case e ^. entitySounds of 
  Nothing -> return ()
  Just sfx -> do
    (L.V3 x y z) <- toWorldPosition $ e ^. entityWorldTransform
    forM_ (sfx ^. sfxSources) $ \s -> do
      AL.sourcePosition s $= AL.Vertex3 (CFloat x) (CFloat y) (CFloat z)
      sState <- liftIO $ AL.sourceState s
      case sState of
        AL.Initial -> AL.play [s]
        _ -> return ()

animateEntity :: Entity s -> Game s ()
animateEntity e = do
  cam <- use gameStateCamera
  vpm <- cameraVP cam
  playEntity e
  drawEntity vpm e
