{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Main where

import           ClassyPrelude as ClassyP
import           Control.Lens
import           Control.Wire.Core
import qualified Data.ObjectName as ON
import           FRP.Netwire
import qualified FRP.Netwire.Input.GLFW as N
import           Game.Entity
import           Game.Entity.Camera
import           Game.Entity.GiantFeaturelessPlane
import           Game.Entity.Player
import           Game.Events
import           Game.Graphics.Model.AssImp
import           Game.Graphics.Shader.Loader
import           Game.Graphics.Texture.Loader
import           Game.Types
import           Game.Wires
import           Game.World.Physics
import           Graphics.Binding
import           Linear as L
import qualified Physics.Bullet as P
import qualified Sound.ALUT as AL
import           Foreign.Resource

updateGLFWInput :: Game s ()
updateGLFWInput = do
  iod <- use gameStateIOData
  is' <- liftIO $ N.pollGLFW (iod ^. ioDataGLFWInputState)
                             (iod ^. ioDataGLFWInputControl)
  gameStateIOData . ioDataGLFWInputState .= is'

stepTime :: Game s (Timed Integer ())
stepTime = do
  sess <- use $ gameStateIOData . ioDataSession
  (timey, sess') <- liftIO $ stepSession sess
  gameStateIOData . ioDataSession .= sess'
  return timey

doGame :: GameState (Timed Integer ()) -> IO ()
doGame initGS = void $ runGame initGS go
  where
    go = do
      updateGLFWInput
      time_ <- stepTime
      mainWire <- concatA <$> use gameStateWires
      _ <- stepWire mainWire time_ (Right ())
      win <- use $ gameStateIOData . ioDataWindow
      liftIO $ swapBuffers win
      unlessM (use gameStateShouldClose)
        go

setupPhysics :: Program -> IO (PhysicsWorld s, Player s, Camera s, Entity s, P.RigidBody)
setupPhysics prog = do
  (theModelE, theModelRB) <- createTheModel prog
  pw <- newPhysicsWorld
  pl' <- newPlayer
  let pl = pl' & playerEntity . entityGraphics .~ (theModelE ^. entityGraphics)
  go <- P.getGhostObject $ pl ^. playerController
  cam <- newCamera go 2
  cameraLookAtTarget cam
  pw' <- addPlayerToPhysicsWorld pl pw
  pw'' <- addCameraToPhysicsWorld cam pw'
  withCameraTransform cam $ \t -> do
    P.setIdentity t
    P.setOrigin t 0 0 (-5)
    setCameraTransform cam t)
  cameraLookAtTarget cam
  giantFeaturelessPlane <- newGiantFeaturelessPlane (L.V3 0 (-3) 0) 0
  pw''' <- addGiantFeaturelessPlaneToPhysicsWorld giantFeaturelessPlane pw''
  pw'''' <- addEntityToPhysicsWorld theModelE pw'''
  setGravityPhysicsWorld (L.V3 0 (-10) 0) pw''''
  P.kccSetGravity (cam ^. cameraController) 0 0 0
  return (pw'''', pl, cam, theModelE, theModelRB)

createTheModel :: Program -> IO (Entity s, P.RigidBody)
createTheModel prog = do
  model <- P.newBoxShape 0.5 0.5 0.5
  startXform <- P.new ((0, 0, 0, 0), (0, 0, 0))
  P.setIdentity startXform
  P.setOrigin startXform 0 1 0
  -- Don't delete the ms!!  See below.
  ms <- P.new startXform
  rbci <- P.newRigidBodyConstructionInfo 1 ms model 0 1 0
  -- The MotionState ms now belongs to the cube.
  rb   <- P.newRigidBody rbci
  P.del startXform
  P.del rbci

  let modelRoot = "res" </> "models" </> "Bayonetta 1"
      modelName = "bayo_default.dae"
      defaultTexture = "res" </> "models" </> "no_texture.png"
  (AssImpScene meshes) <- loadAssImpScene $ modelRoot </> modelName
  vaoData <- forM meshes (\aim -> do
                             dif <- loadPNGTexture $
                                    maybe defaultTexture (modelRoot </>) $
                                    aim ^. assImpMeshTextureBundle . textureBundleDiffuseTexture
                             return $ VaoData
                               (_assImpMeshVAO aim)
                               prog
                               Triangles
                               (_assImpMeshIndexNum aim)
                               (emptyTextureBundle & textureBundleDiffuseTexture .~ Just dif)
                               (_assImpMeshShaderMaterial aim))


  src :: AL.Source <- ON.genObjectName
  sbuf <- AL.createBuffer (AL.File $ "res" </> "sound" </> "africa-toto.wav")
  AL.buffer src $= Just sbuf

  let e = Entity
          { _entityChildren = empty
          , _entityGraphics = Just Gfx
            { _gfxVaoData = vaoData
            , _gfxChildren = empty
            }
          , _entitySounds = Just Sfx
            { _sfxSources = singleton src }
          , _entityLogic = Just Lfx
            { _lfxScripts = fromList
              [ \model_ -> do
                  entityLocalClosestRayCast model_ (L.V3 0 (-2) 0) $
                    const $ setEntityLinearVelocity model_ (L.V3 0 6 0)
                  return model_
              ]
            }
          , _entityCollisionObject = CollisionObject $ P.toCollisionObject rb
          , _entityRigidBody = Just $ RigidBody rb
          }
  return (e, rb)

concatA :: ArrowPlus a => Vector (a b b) -> a b b
concatA = foldr (<+>) id

setupWritableBuffers :: Program -> IO WritableBufferBundle
setupWritableBuffers prog = do
  -- Do point lights
  let pointLight = PointLight
                   { _pointLightPosition = V4 100 100 0 1
                   , _pointLightIntensity = 1
                   }
      pointLightBundle = PointLightBundle
                         { _pointLightBundleLights = singleton pointLight
                         , _pointLightBundleNum = 1
                         }

  plbpb <- genName'
  writableBufferWrite pointLightBundle plbpb
  uniform prog PointLightBlock plbpb
  bindBlock PointLightBlock plbpb

  smpb <- genName'
--  uniform prog ShaderMaterialBlock smpb

  --persistentBufferWrite 1000 (ShaderMaterial (L.V3 1 1 1) (L.V3 1 1 1) (L.V3 1 1 1) 1 1) smpb
  --bindBlock ShaderMaterialBlock smpb

  cpb  <- genName'
  uniform prog CameraBlock cpb
  --persistentBufferWrite 1000 (ShaderCamera L.identity L.identity L.identity) cpb
  --bindBlock CameraBlock cpb

  return WritableBufferBundle
    { _writableBufferBundleShaderCameraBuffer = cpb
    , _writableBufferBundleShaderMaterialBuffer = smpb
    , _writableBufferBundlePointLightBundleBuffer = plbpb
    }

gameMain :: IO ()
gameMain = AL.withProgNameAndArgs AL.runALUT $ \_progName _args ->
  withGraphicsContext defaultGraphicsContext . withWindow defaultWindowConfig
  $ \win -> do
  contextCurrent $= Just win

  --cullFace $= Just Back
  depthFunc $= Just DepthLess
  -- FIXME: we will eventually have to free the function pointer
  -- that mkGLDEBUGPROC gives us!!!
  debugMessageCallback $= Just simpleDebugFunc

  clearColor $= color4 0 0 0.4 0
  -- GL.viewport $= (GL.Position 0 0, GL.Size 1920 1080)
  mdev <- AL.openDevice Nothing
  let dev = fromMaybe (error "Couldn't open the sound device.") mdev
  mctxt <- AL.createContext dev []
  let ctxt = fromMaybe (error "Couldn't create the sound context.") mctxt
  AL.currentContext $= Just ctxt

  prog <- compileShaders

  AL.distanceModel $= AL.InverseDistance
  (physicsWorld, player, cam, _, _) <- setupPhysics prog

  buffBundle <- setupWritableBuffers prog

  ic <- N.mkInputControl win
  input <- liftIO $ N.getInput ic
  let sess = countSession_ 1
      ioData = initIOData & ioDataGLFWInputControl .~ ic
                          & ioDataGLFWInputState .~ input
                          & ioDataSession .~ sess
                          & ioDataWindow .~ win

      animationWire :: GameEffectWire s
      animationWire = effectWire $ do
        clear $ defaultClearBuffer & clearBufferColor .~ True
                                   & clearBufferDepth .~ True
        entities <- use $ gameStatePhysicsWorld . physicsWorldEntities
        entities' <- mapM animateEntity entities
        gameStatePhysicsWorld . physicsWorldEntities .= entities'

      physicsWire :: GameEffectWire s
      physicsWire = effectWire $ do
        pw <- use gameStatePhysicsWorld
        stepPhysicsWorld pw

      mainWires = fromList [ animationWire
                           , playerHorizontalMovement >>> movePlayer
                           , physicsWire
                           , close
                           , jump
                           , camera
                           , zoomCamera
                           , turnPlayer
                           ]

      gameState = initGameState & gameStatePhysicsWorld .~ physicsWorld
                                & gameStatePlayer .~ player
                                & gameStateCamera .~ cam
                                & gameStateSoundDevice .~ dev
                                & gameStateSoundContext .~ ctxt
                                & gameStateWires .~ mainWires
                                & gameStateIOData .~ ioData
                                & gameStateWritableBufferBundle .~ buffBundle
  doGame gameState
