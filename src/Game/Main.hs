{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Main where

import           ClassyPrelude
import           Control.Concurrent
import qualified Control.Wire.Core           as N
import qualified FRP.Netwire                 as N
import qualified FRP.Netwire.Input           as N
import qualified FRP.Netwire.Input.GLFW      as N
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

gameMain :: IO (((), N.GLFWInputState), GameState)
gameMain = withGraphicsContext defaultGraphicsContext
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
  let mainWire = printOut

  let someFunc' :: Game ()
      someFunc' = do
        let sess = N.countSession_ 1
        loop win sess mainWire
        liftIO $ deleteObjectName vao
        liftIO $ deleteObjectName prog
        liftIO $ G.terminate
          where
            loop :: G.Window
                 -> (N.Session Game (N.Timed Integer ()))
                 -> GameWire (N.Timed Integer ()) () b
                 -> Game ()
            loop w sess_ wire = do
              sc <- liftIO $ G.windowShouldClose w
              (s, sess_') <- N.stepSession sess_
              (_, newWire) <- N.stepWire wire s (Right ())
              unless sc $ loop w sess_' newWire

  ic <- N.mkInputControl win
  runGame initGameState ic someFunc'
{-
game :: Game ( NamedHandler ()
             , IO ()
             , ThreadId )
-}
