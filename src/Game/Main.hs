{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Main where

import           ClassyPrelude
import           Control.Lens
import           Control.Wire.Core
import           FRP.Netwire hiding (when)
import qualified FRP.Netwire.Input.GLFW      as N
import           Game.Types
import           Game.Graphics.Model.Loader
import           Game.Graphics.Rendering
import           Game.Graphics.Shader.Loader
import           Game.Graphics.Texture.Loader
import           Game.Events
import           Game.World.Physics
import           Game.Entity.Camera
import           Game.Entity.Player
import           Game.Entity.GiantFeaturelessPlane
import           Graphics.Binding
import           Linear                      as L
import qualified Physics.Bullet as P
import qualified Plugin.Load as PL
import           System.Exit

{-
printContextVersion :: MonadIO m => G.Window -> m ()
printContextVersion win = liftIO $ do
  maj <- G.getWindowContextVersionMajor win
  min_ <- G.getWindowContextVersionMinor win
  rev <- G.getWindowContextVersionRevision win
  printf "%i.%i.%i\n" maj min_ rev
-}

setupPhysics :: IO (PhysicsWorld, Player, Camera)
setupPhysics = do
  pw <- newPhysicsWorld
  pl <- newPlayer
  go <- P.getGhostObject $ pl ^. playerController
  cam <- newCamera go 10
  cameraLookAtTarget cam
  pw' <- addPlayerToPhysicsWorld pl pw
  pw'' <- addCameraToPhysicsWorld cam pw'
  withCameraTransform cam
    (\t -> do
        P.setIdentity t
        P.setOrigin t 0 0 (-5)
        setCameraTransform cam t)
  cameraLookAtTarget cam
  giantFeaturelessPlane <- newGiantFeaturelessPlane (L.V3 0 (-10) 0) 0
  pw''' <- addGiantFeaturelessPlaneToPhysicsWorld giantFeaturelessPlane pw''
  setGravityPhysicsWorld (L.V3 0 (-10) 0) pw'''
  P.kccSetGravity (cam ^. cameraController) 0 0 0
  return (pw''', pl, cam)

gameMain :: IO ()
gameMain = withGraphicsContext defaultGraphicsContext
           . withWindow defaultWindowConfig
           $ \win -> do
  contextCurrent $= Just win

  -- cullFace $= Just Back
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

  (physicsWorld, player, cam) <- setupPhysics
  let gameState = initGameState & gameStatePhysicsWorld .~ physicsWorld
                                & gameStatePlayer .~ player
                                & gameStateCamera .~ cam

  let renderWire :: TextureTarget t => GameWire s (Program, TextureUnit, VertexArrayObject, Int, TextureObject t) ()
      renderWire = mkGen_ (\(p, tu, vao_, n, tex_) -> Right <$> do
                              gs <- use simple
                              render gs p tu vao_ n tex_)

      physicsWire :: GameWire s a ()
      physicsWire = mkGen_ $ const $ Right <$> do
        pw <- use gameStatePhysicsWorld
        p  <- use gameStatePlayer
        c  <- use gameStateCamera
        void $ stepPhysicsWorld pw
        cameraLookAtTarget c
        print =<< getPlayerPosition p
        print =<< getCameraPosition c

  -- proof of concept.
  moveForward <- PL.loadPlugin "scripts/" "Movement" "moveForward"
  -- proof of double-loading.
  moveBackward <- PL.loadPlugin "scripts/" "Movement" "moveBackward"

  let mainWire = renderWire <+>
                 moveForward <+>
                 moveBackward <+>
                 moveLeft <+>
                 moveRight <+>
                 physicsWire <+>
                 close <+>
                 jump <+>
--                 camera <+>
--                 reloadPlugins
                 zoomCamera

  ic <- N.mkInputControl win
  let sess = countSession_ 1
  input <- liftIO $ N.getInput ic
  let doGame :: N.GLFWInputState
             -> Session IO (Timed Integer ())
             -> GameWire (Timed Integer ()) ( Program
                                            , TextureUnit
                                            , VertexArrayObject
                                            , Int
                                            , TextureObject TextureTarget2D ) b
             -> GameState (Timed Integer ())
             -> IO ((b, N.GLFWInputState), GameState (Timed Integer ()))
      doGame input_ sess_ wire gs = do
        void $ N.pollGLFW input_ ic
        (timeState, sess') <- stepSession sess_
        let game = stepWire wire timeState
                   (Right (prog, texSampleLoc, vao, snd ebuf, tex))
        (((_, wire'), input'), gs') <- runGame gs ic game
        swapBuffers win
        when (gs ^. gameStateShouldClose) $
          exitSuccess
        doGame input' sess' wire' gs'
  void $ doGame input sess mainWire gameState
