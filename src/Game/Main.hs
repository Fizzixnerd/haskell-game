{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Main where

import           ClassyPrelude
import           Control.Lens
import           Control.Wire.Core
import qualified Data.ObjectName as ON
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
import qualified Sound.OpenAL.AL as AL
import qualified Sound.ALUT as AL

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

createTheCube :: IO (Entity (Timed Integer ()), P.RigidBody)
createTheCube = do
  cube <- P.newBoxShape 0.5 0.5 0.5
  startXform <- P.new ((0, 0, 0, 0), (0, 0, 0))
  P.setIdentity startXform
  P.setOrigin startXform 0 1 0
  -- Don't delete this!!  See below.
  ms <- P.new startXform
  rbci <- P.newRigidBodyConstructionInfo 1 ms cube 0 1 0
  -- The MotionState ms now belongs to the cube.
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

  src :: AL.Source <- ON.genObjectName
  sbuf <- AL.createBuffer (AL.File "res/sound/africa-toto.wav")
  AL.buffer src $= Just sbuf

  let e = Entity
          { _entityGraphics = Just Gfx
            { _gfxVaoData = singleton (vao, prog, Triangles, fromIntegral $ snd ebuf)
            , _gfxTextureBlob = GfxTexture () (Just (Simple2DSampler, tex)) ()
            , _gfxChildren   = empty
            }
          , _entitySounds = Just Sfx
            { _sfxSources = singleton src }
          , _entityLogic = Just Lfx
            { _lfxScripts = fromList [\cube_ -> do
                                         t <- use gameStateTime
                                         when (t < 5.5) $ do
                                           setEntityLinearVelocity cube_ (L.V3 4 4 4)
                                         return cube_
                                     ,\cube_ -> do
                                         entityLocalClosestRayCast cube_ (L.V3 0 (-2) 0) $ \_ -> do
                                           setEntityLinearVelocity cube_ (L.V3 0 4 0)
                                         return cube_
                                     ]
            }
          , _entityWorldTransform = WorldTransform $ P.toCollisionObject rb
          , _entityRigidBody = Just $ RigidBody rb
          }
  return (e, rb)

gameMain :: IO ()
gameMain = AL.withProgNameAndArgs AL.runALUT $ \_progName _args -> do
  withGraphicsContext defaultGraphicsContext
    . withWindow defaultWindowConfig
    $ \win -> do
    contextCurrent $= Just win
    
    --cullFace $= Just Back
    depthFunc $= Just DepthLess
    -- Remember: we will eventually have to free the function pointer
    -- that mkGLDEBUGPROC gives us!!!
    debugMessageCallback $= Just simpleDebugFunc
    
    clearColor $= color4 0 0 0.4 0
    -- GL.viewport $= (GL.Position 0 0, GL.Size 1920 1080)
    mdev <- AL.openDevice Nothing
    let dev = case mdev of
          Nothing -> error "Couldn't open the sound device."
          Just dev_ -> dev_
    mctxt <- AL.createContext dev []
    let ctxt = case mctxt of
          Nothing -> error "Couldn't create the sound context."
          Just ctxt_ -> ctxt_
    AL.currentContext $= Just ctxt

    AL.distanceModel $= AL.InverseDistance
    (physicsWorld', player, cam) <- setupPhysics
    (theCubeE, theCubeRB) <- createTheCube
    physicsWorld <- addRigidBodyToPhysicsWorld theCubeRB physicsWorld'

    let gameState = initGameState & gameStatePhysicsWorld .~ physicsWorld
                                  & gameStatePlayer .~ player
                                  & gameStateCamera .~ cam
                                  & gameStateEntities .~ singleton theCubeE
                                  & gameStateSoundDevice .~ dev
                                  & gameStateSoundContext .~ ctxt
        animationWire :: GameWire s a ()
        animationWire = mkGen_ $ const $ Right <$> do
          entities <- use gameStateEntities
          entities' <- mapM animateEntity entities
          gameStateEntities .= entities'

        physicsWire :: GameWire s a ()
        physicsWire = mkGen_ $ const $ Right <$> do
          pw <- use gameStatePhysicsWorld
          void $ stepPhysicsWorld pw

        mainWire =     animationWire
                   <+> (playerHorizontalMovement >>> movePlayer)
                   <+> physicsWire
                   <+> close
                   <+> jump
                   <+> camera
                   <+> zoomCamera
                   <+> turnPlayer
                   <+> (timeF >>> updateTime)

    ic <- N.mkInputControl win
    input <- liftIO $ N.getInput ic
    let sess = countSession_ 1
        doGame :: N.GLFWInputState
               -> Session IO (Timed Integer ())
               -> GameWire (Timed Integer ()) Double b
               -> GameState (Timed Integer ())
               -> IO ((b, N.GLFWInputState), GameState (Timed Integer ()))
        doGame input_ sess_ wire gs = do
          void $ N.pollGLFW input_ ic
          (timeState, sess') <- stepSession sess_
          let game = stepWire wire timeState (Right 0)
          (((b, wire'), input'), gs') <- runGame gs ic game
          swapBuffers win
          if gs' ^. gameStateShouldClose
            then return ((either 
                          (const $ error "mainWire inhibited and exited. (why!?)")
                          id
                          b, input'), gs')
            else doGame input' sess' wire' gs'
    void $ doGame input sess mainWire gameState
