{-# LANGUAGE NoImplicitPrelude #-}

module Game.Graphics.Rendering where

import           ClassyPrelude
import           Control.Lens
import qualified Data.Vector.Storable                 as VS
import           Foreign
import           Foreign.C.Types
import           Game.Types
import qualified Graphics.Rendering.OpenGL.GL         as G
import qualified Graphics.Rendering.OpenGL.GLU.Errors as G (errors)
import qualified Linear.OpenGL                        as L ()

printGLErrors :: MonadIO m => m ()
printGLErrors = liftIO G.errors >>= mapM_ print

render :: MonadIO m =>
          GameState
       -> G.Program
       -> G.UniformLocation
       -> G.UniformLocation
       -> G.VertexArrayObject
       -> (G.BufferObject, Int) -- ^ Element buffer with its length
       -> G.TextureObject
       -> m ()
render gs prog mvpLoc texSampleLoc vao (_, lene) tex = liftIO $ do
  G.clear [G.ColorBuffer, G.DepthBuffer]
  G.currentProgram G.$= Just prog

  G.bindVertexArrayObject G.$= Just vao

  G.activeTexture G.$= G.TextureUnit 0
  G.textureBinding G.Texture2D G.$= Just tex
  G.uniform texSampleLoc G.$= G.TextureUnit 0
  G.uniform mvpLoc G.$= (gs ^. gameStateCamera . cameraMVP)

  G.drawElements G.Triangles (fromIntegral lene) G.UnsignedInt nullPtr
--  printGLErrors -- :(

unsafeWithVecLen :: (Storable a, MonadIO m) => VS.Vector a -> (Ptr a -> Int -> IO b) -> m b
unsafeWithVecLen vec f = liftIO $ do
  let (fptr, vecLen) = VS.unsafeToForeignPtr0 vec
  withForeignPtr fptr $ \ptr -> f ptr vecLen

bufferData :: MonadIO m =>
              G.AttribLocation
           -> G.AttribLocation
           -> G.AttribLocation
           -> VS.Vector VTNPoint
           -> VS.Vector CUInt
           -> m (G.VertexArrayObject, G.BufferObject, (G.BufferObject, Int))
bufferData vtxLoc texLoc nmlLoc objPoints objIndices = liftIO $ do
  vao <- G.genObjectName
  G.bindVertexArrayObject G.$= Just vao

  vbuf <- G.genObjectName
  G.bindBuffer G.ArrayBuffer G.$= Just vbuf
  unsafeWithVecLen objPoints $
    \vtxs lenv -> G.bufferData G.ArrayBuffer G.$= ( fromIntegral $ lenv * sizeOf (0 :: CFloat)
                                                  , vtxs
                                                  , G.StaticDraw )

  let vadvOffset = intPtrToPtr . IntPtr $ 0
      vadtOffset = intPtrToPtr . IntPtr . fromIntegral $ 4 * sizeOf (0 :: CFloat)
      vadnOffset = intPtrToPtr . IntPtr . fromIntegral $ 6 * sizeOf (0 :: CFloat)
      vadv = G.VertexArrayDescriptor 4 G.Float 0 vadvOffset
      vadt = G.VertexArrayDescriptor 2 G.Float 0 vadtOffset
      vadn = G.VertexArrayDescriptor 3 G.Float 0 vadnOffset
  G.vertexAttribArray vtxLoc G.$= G.Enabled
  G.vertexAttribPointer vtxLoc G.$= (G.ToFloat, vadv)
  G.vertexAttribArray texLoc G.$= G.Enabled
  G.vertexAttribPointer texLoc G.$= (G.ToFloat, vadt)
  G.vertexAttribArray nmlLoc G.$= G.Enabled
  G.vertexAttribPointer nmlLoc G.$= (G.ToFloat, vadn)

  ebuf <- G.genObjectName
  G.bindBuffer G.ElementArrayBuffer G.$= Just ebuf
  lene <- unsafeWithVecLen objIndices $
          \elts lene -> do
            G.bufferData G.ElementArrayBuffer G.$= ( fromIntegral $ lene * sizeOf (0 :: CUInt)
                                                   , elts
                                                   , G.StaticDraw )
            return lene

  return (vao, vbuf, (ebuf, lene))
