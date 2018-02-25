{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Entity where

import ClassyPrelude
import Game.Types
import Graphics.Binding
import Game.Graphics.Texture.Loader
import Game.Graphics.Shader.Loader
import qualified Linear as L
import Control.Lens
import qualified Physics.Bullet as P
import Foreign.C.Types

toWorldMatrix :: MonadIO m => WorldTransform -> m (L.M44 Float)
toWorldMatrix wt = do
  liftIO $ bracket (P.coAllocateWorldTransform $ wt ^. unWorldTransform) P.del $
    \t -> do
      (CFloat i, CFloat j, CFloat k, CFloat r) <- P.getRotation t
      (CFloat x, CFloat y, CFloat z) <-  P.getOrigin t
      return $ L.mkTransformation (L.Quaternion r (L.V3 i j k)) (L.V3 x y z)

bindGfxTexture :: MonadIO m => GfxTexture -> m ()
bindGfxTexture GfxTexture {..} = traverse_ (uncurry . flip $ texture) _gfxTexture2D

drawGfx :: VPMatrix -> Gfx s -> Game s ()
drawGfx vpm gfx = do
  bindGfxTexture (gfx ^. gfxTextureBlob)
  forM_ (gfx ^. gfxVaoData) $ \(vao, prog, mode, size) -> do
    clear $ defaultClearBuffer & clearBufferColor .~ True
                               & clearBufferDepth .~ True
    useProgram prog
    currentVertexArrayObject $= Just vao
    wrld :: L.M44 Float <- toWorldMatrix $ gfx ^. gfxWorldXform
    uniform prog UniformMVP (vpm L.!*! wrld)
    drawElements mode (fromIntegral size) UnsignedInt
  mapM_ (drawGfx vpm) $ gfx ^. gfxChildren

drawEntity :: VPMatrix -> Entity s -> Game s ()
drawEntity vp Entity {..} = mapM_ (drawGfx vp) _entityGraphics
