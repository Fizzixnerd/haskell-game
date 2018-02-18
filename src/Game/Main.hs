{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Main where

import           ClassyPrelude
import           Control.Concurrent
import           Game.Events
import           Game.Types
import           Game.Graphics.Model.Loader
import           Game.Graphics.Rendering
import           Game.Graphics.Shader.Loader
import           Game.Graphics.Texture.Loader
import qualified Graphics.UI.GLFW as G
import qualified Linear                       as L
import           Text.Printf
import           Game.Graphics.Binding

printContextVersion :: MonadIO m => G.Window -> m ()
printContextVersion win = liftIO $ do
  maj <- G.getWindowContextVersionMajor win
  min_ <- G.getWindowContextVersionMinor win
  rev <- G.getWindowContextVersionRevision win
  printf "%i.%i.%i\n" maj min_ rev

doItAndGimmeFireThing :: Game ()
doItAndGimmeFireThing = withGraphicsContext defaultGraphicsContext
                        . withWindow defaultWindowConfig
                        $ \win -> do
  contextCurrent $= Just win

  cullFace $= Just Back
  depthFunc $= Just DepthLess
--- Remember: we will eventually have to free the function pointer that mkGLDEBUGPROC gives us!!!
  debugMessageCallback $= Just simpleDebugFunc
  printContextVersion win

  prog <- compileShaders

  (objPoints, objIndices) <- loadObjVTN "res/models/simple-cube-2.obj"

  tex <- loadBMPTexture "res/models/simple-cube-2.bmp"

  let posLocation = AttribLocation 0
      texLocation = AttribLocation 1
      nmlLocation = AttribLocation 2
  (vao, _, ebuf) <- bufferData posLocation texLocation nmlLocation objPoints objIndices

  clearColor $= color4 0 0 0.4 0
--  GL.viewport $= (GL.Position 0 0, GL.Size 1920 1080)

  let texSampleLoc = TextureUnit 0
  --(hello, shouldClose, key, mouseData, tick) <- compileGameNetwork prog texSampleLoc vao ebuf tex

  let someFunc' :: IO ()
      someFunc' = do
        loop win
        deleteObjectName vao
        deleteObjectName prog
        G.terminate
          where
            loop w = do
              sc <- G.windowShouldClose w
              unless sc $ loop w

  return ()
{-
game :: Game ( NamedHandler ()
             , IO ()
             , ThreadId )
-}
someFunc :: IO ()
someFunc = join (runGame game)
