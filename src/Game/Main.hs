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
import           Game.Entity
import           Graphics.Binding
import           Linear                      as L
import qualified Physics.Bullet as P
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
  cam <- newCamera go 4
  cameraLookAtTarget cam
  pw' <- addPlayerToPhysicsWorld pl pw
  pw'' <- addCameraToPhysicsWorld cam pw'
  withCameraTransform cam
    (\t -> do
        P.setIdentity t
        P.setOrigin t 0 0 (-5)
        setCameraTransform cam t)
  cameraLookAtTarget cam
  giantFeaturelessPlane <- newGiantFeaturelessPlane (L.V3 0 (-3) 0) 0
  pw''' <- addGiantFeaturelessPlaneToPhysicsWorld giantFeaturelessPlane pw''
  setGravityPhysicsWorld (L.V3 0 (-10) 0) pw'''
  P.kccSetGravity (cam ^. cameraController) 0 0 0
  return (pw''', pl, cam)

-- plugins :: [PL.Plugin]
-- plugins = [ PL.Plugin "scripts/" "Movement" "moveForward"
--           , PL.Plugin "scripts/" "Movement" "moveBackward"
--           , PL.Plugin "scripts/" "Movement" "moveLeft"
--           , PL.Plugin "scripts/" "Movement" "moveRight"
--           , PL.Plugin "scripts/" "Util"     "reloadPlugins"
--           ]

createTheCube :: IO (Entity s, P.RigidBody)
createTheCube = do
  cube <- P.newBoxShape 0.5 0.5 0.5
  startXform <- P.new ((0, 0, 0, 0), (0, 0, 0))
  P.setIdentity startXform
  P.setOrigin startXform 0 1 0
  -- Don't delete this!!  See below.
  ms <- P.new startXform
  rbci <- P.newRigidBodyConstructionInfo 1 ms cube 0 1 0
  -- The ms now belongs to the cube.
  rb   <- P.newRigidBody rbci
  P.del startXform
  P.del rbci

  prog <- compileShaders

  (objPoints, objIndices) <- loadObjVTN "res/models/simple-cube-2.obj"

  tex <- loadBMPTexture "res/models/simple-cube-2.bmp"

  let posLocation = AttribLocation 0
      texLocation = AttribLocation 1
      nmlLocation = AttribLocation 2
  (vao, _, ebuf) <- bufferData posLocation texLocation nmlLocation objPoints objIndices

  let e = Entity
          { _entityGraphics = Just Gfx
            { _gfxVaoData = singleton (vao, prog, Triangles, fromIntegral $ snd ebuf)
            , _gfxTextureBlob = GfxTexture () (Just (Simple2DSampler, tex)) ()
            , _gfxChildren   = empty
            , _gfxWorldXform = WorldTransform $ P.toCollisionObject rb
            }
          , _entitySounds = Nothing
          , _entityLogic = Nothing
          }
  return (e, rb)

gameMain :: IO ()
gameMain = withGraphicsContext defaultGraphicsContext
           . withWindow defaultWindowConfig
           $ \win -> do
  contextCurrent $= Just win

  --cullFace $= Just Back
  depthFunc $= Just DepthLess
  -- Remember: we will eventually have to free the function pointer
  -- that mkGLDEBUGPROC gives us!!!
  debugMessageCallback $= Just simpleDebugFunc
  --  printContextVersion win

  clearColor $= color4 0 0 0.4 0
  -- GL.viewport $= (GL.Position 0 0, GL.Size 1920 1080)

  (physicsWorld', player, cam) <- setupPhysics
  (theCubeE, theCubeRB) <- createTheCube
  physicsWorld <- addRigidBodyToPhysicsWorld theCubeRB physicsWorld'

  let gameState = initGameState & gameStatePhysicsWorld .~ physicsWorld
                                & gameStatePlayer .~ player
                                & gameStateCamera .~ cam
                                & gameStateEntities .~ singleton theCubeE

  let renderWire :: GameWire s a ()
      renderWire = mkGen_ $ const $ Right <$> do
        entities <- use gameStateEntities
        cam_ <- use gameStateCamera
        vp <- cameraVP cam_
        mapM_ (drawEntity vp) entities

      physicsWire :: GameWire s a ()
      physicsWire = mkGen_ $ const $ Right <$> do
        pw <- use gameStatePhysicsWorld
        void $ stepPhysicsWorld pw

  -- -- proof of concept.
  -- moveForward   <- PL.loadPlugin $ PL.Plugin "scripts/" "Movement" "moveForward"
  -- -- proof of double-loading.
  -- moveBackward  <- PL.loadPlugin $ PL.Plugin "scripts/" "Movement" "moveBackward"
  -- moveLeft      <- PL.loadPlugin $ PL.Plugin "scripts/" "Movement" "moveLeft"
  -- moveRight     <- PL.loadPlugin $ PL.Plugin "scripts/" "Movement" "moveRight"
  -- holy shit I'm so fucking mad fuck this shit it doesn't work
  -- sometimes wtf.
  --  reloadPlugins <- PL.loadPlugin $ PL.Plugin "scripts/" "Util"     "reloadPlugins"

  let mainWire =     renderWire
                 <+> (playerHorizontalMovement >>> movePlayer)
                 <+> physicsWire
                 <+> close
                 <+> jump
                 <+> camera
                 <+> zoomCamera
                 <+> turnPlayer

  ic <- N.mkInputControl win
  let sess = countSession_ 1
  input <- liftIO $ N.getInput ic
  let doGame :: N.GLFWInputState
             -> Session IO (Timed Integer ())
             -> GameWire (Timed Integer ()) () b
             -> GameState (Timed Integer ())
             -> IO ((b, N.GLFWInputState), GameState (Timed Integer ()))
      doGame input_ sess_ wire gs = do
        void $ N.pollGLFW input_ ic
        (timeState, sess') <- stepSession sess_
        let game = stepWire wire timeState (Right ())
        (((_, wire'), input'), gs') <- runGame gs ic game
        swapBuffers win
        when (gs ^. gameStateShouldClose) $
          exitSuccess
        doGame input' sess' wire' gs'
  void $ doGame input sess mainWire gameState
