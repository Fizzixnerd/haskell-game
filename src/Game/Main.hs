{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Main where

import           ClassyPrelude
import           Control.Wire.Core
import           FRP.Netwire
import qualified FRP.Netwire.Input.GLFW      as N
import           Game.Types
import           Game.Graphics.Model.Loader
import           Game.Graphics.Rendering
import           Game.Graphics.Shader.Loader
import           Game.Graphics.Texture.Loader
import           Game.Events
import qualified Graphics.UI.GLFW            as G
import           Text.Printf
import           Graphics.Binding
import           Control.Lens

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
  -- Remember: we will eventually have to free the function pointer that mkGLDEBUGPROC gives us!!!
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
  -- GL.viewport $= (GL.Position 0 0, GL.Size 1920 1080)

  let texSampleLoc = TextureUnit 0
  
  let renderWire :: TextureTarget t => GameWire s (Program, TextureUnit, VertexArrayObject, Int, TextureObject t) ()
      renderWire = mkGen_ (\(p, tu, vao_, n, tex_) -> do
                              gs <- use simple
                              Right <$> (render gs p tu vao_ n tex_))
  let mainWire = renderWire <+>
                 moveForward <+>
                 moveBackward <+>
                 moveLeft <+>
                 moveRight <+>
                 camera

  ic <- N.mkInputControl win
  let sess = countSession_ 1
  input <- liftIO $ N.getInput ic
  let doGame :: N.GLFWInputState
             -> Session IO (Timed Integer ())
             -> GameWire (Timed Integer ()) (Program, TextureUnit, VertexArrayObject, Int, TextureObject TextureTarget2D) b
             -> GameState (Timed Integer ())
             -> IO ((b, N.GLFWInputState), GameState (Timed Integer ()))
      doGame input_ sess_ wire gs = do
        void $ N.pollGLFW input_ ic
        (timeState, sess') <- stepSession sess_
        let game = stepWire wire timeState (Right (prog, texSampleLoc, vao, snd ebuf, tex))
        (((_, wire'), input'), gs') <- runGame gs ic game
        G.swapBuffers win
        doGame input' sess' wire' gs'
  void $ doGame input sess mainWire initGameState
