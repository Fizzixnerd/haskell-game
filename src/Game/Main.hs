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
import qualified Linear                      as L
import           Text.Printf
import           Game.Graphics.Binding

{-
printContextVersion :: MonadIO m => G.Window -> m ()
printContextVersion win = liftIO $ do
  maj <- G.getWindowContextVersionMajor win
  min_ <- G.getWindowContextVersionMinor win
  rev <- G.getWindowContextVersionRevision win
  printf "%i.%i.%i\n" maj min_ rev
-}

gameMain :: IO ()
gameMain = withGraphicsContext defaultGraphicsContext
           . withWindow defaultWindowConfig
           $ \win -> do
  contextCurrent $= Just win

  cullFace $= Just Back
  depthFunc $= Just DepthLess
--- Remember: we will eventually have to free the function pointer that mkGLDEBUGPROC gives us!!!
  debugMessageCallback $= Just simpleDebugFunc
--  printContextVersion win

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

  ic <- N.mkInputControl win
  let sess = N.countSession_ 1
  input <- liftIO $ N.getInput ic
  let doGame :: N.GLFWInputState
             -> N.Session IO (N.Timed Integer ())
             -> GameWire (N.Timed Integer ()) () b
             -> GameState
             -> IO ((b, N.GLFWInputState), GameState)
      doGame input_ sess_ wire gs = do
        input' <- N.pollGLFW input_ ic
        (timeState, sess') <- N.stepSession sess
        let game = N.stepWire wire timeState (Right ())
        (((result, wire), input''), gs') <- runGame gs ic game
        doGame input'' sess' wire gs'
  void $ doGame input sess mainWire initGameState
