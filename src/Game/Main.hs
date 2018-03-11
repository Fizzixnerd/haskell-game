{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
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
import           Foreign.Resource
import           Game.Entity
import           Game.Entity.Camera
import           Game.Entity.GiantFeaturelessPlane
import           Game.Entity.Player
import           Game.Events
import           Game.Graphics.Model.AssImp
import           Game.Graphics.Shader.Loader
import           Game.Graphics.Texture.Loader
import           Game.Graphics.Types
import           Game.Types
import           Game.Wires
import           Game.World.Physics
import           Graphics.Binding
import           Linear as L
import qualified Physics.Bullet as P
import qualified Sound.ALUT as AL
import           Foreign.Resource
import           Game.Entity.Animate

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

doGame :: GameState (Timed Integer ()) -> ResIO ()
doGame initGS = void $ runGame initGS go
  where
    go = do
      updateGLFWInput
      time_ <- stepTime
      mainWire <- concatA <$> use gameStateWires
      _ <- stepWire mainWire time_ (Right ())
      win <- use $ gameStateIOData . ioDataWindow
      liftIO $ swapBuffers win
      unlessM (use gameStateShouldClose) go

setupPhysics :: (ShaderPipeline, ShaderPipeline) -> IO (PhysicsWorld s, Player s, Camera s, Entity s, Entity s)
setupPhysics ps = do
  (theModel1E, theModel2E) <- createTheModel ps
  pw <- newPhysicsWorld
  pl' <- newPlayer
  let pl = pl' & playerEntity . entityGraphics .~ (theModel1E ^. entityGraphics)
      con = pl ^. playerController
  cam <- newCamera con 4
  pw' <- addPlayerToPhysicsWorld pl pw
  pw'' <- addCameraToPhysicsWorld cam pw'
  withCameraTransform cam $ \t -> do
    P.setIdentity t
    P.setOrigin t 0 1 5
    setCameraTransform cam t
  giantFeaturelessPlane <- newGiantFeaturelessPlane (L.V3 0 (-3) 0) 0

  pw''' <- addGiantFeaturelessPlaneToPhysicsWorld giantFeaturelessPlane pw''
  pw'''' <- addEntityToPhysicsWorld theModel1E pw'''
  pw''''' <- addEntityToPhysicsWorld theModel2E pw''''
  setGravityPhysicsWorld (L.V3 0 (-10) 0) pw'''''
  P.rbSetGravity (cam ^. cameraController) 0 0 0
  return (pw''''', pl, cam, theModel1E, theModel2E)

createTheModel :: (ShaderPipeline, ShaderPipeline) -> IO (Entity s, Entity s)
createTheModel (phong, normalMap) = do
  model <- P.newBoxShape 0.5 0.5 0.5
  startXform <- P.new ((0, 0, 0, 0), (0, 0, 0))
  P.setIdentity startXform
  P.setOrigin startXform 0 1 0
  -- Don't delete the ms!!  See below.
  ms <- P.new startXform
  rbci <- P.newRigidBodyConstructionInfo 1 ms model 0 1 0
  -- The MotionState ms now belongs to the cube.
  rb <- P.newRigidBody rbci
  P.del startXform
  P.del rbci

  model' <- P.newBoxShape 0.5 0.5 0.5
  startXform' <- P.new ((0, 0, 0, 0), (0, 0, 0))
  P.setIdentity startXform'
  P.setOrigin startXform' 4 1 0
  -- Don't delete the ms!!  See below.
  ms' <- P.new startXform'
  rbci' <- P.newRigidBodyConstructionInfo 0 ms' model' 0 1 0
  -- The MotionState ms now belongs to the cube.
  rb' <- P.newRigidBody rbci'
  P.del startXform'
  P.del rbci'

  let loadMeshes modelRoot defaultTexture aim = do
        dif <- loadPngTexture $
               maybe defaultTexture (modelRoot </>) $
               aim ^. assImpMeshTextureBundle . textureBundleDiffuseTexture
        norm_ <- sequence $
                 loadPngTexture . (modelRoot </>) <$>
                 aim ^. assImpMeshTextureBundle . textureBundleNormalTexture
        let pipeline = if isJust norm_
                       then normalMap
                       else phong
        return $ VaoData
          (_assImpMeshVAO aim)
          pipeline
          Triangles
          (_assImpMeshIndexNum aim)
          (emptyTextureBundle & textureBundleDiffuseTexture .~ Just dif
                              & textureBundleNormalTexture .~ norm_)
          (_assImpMeshShaderMaterial aim)

  let modelRoot = "res" </> "models" </> "Bayonetta 1"
      modelName = "bayo_default.dae"
      defaultTexture = "res" </> "models" </> "simple-cube-2.bmp"
  (AssImpScene meshes) <- loadAssImpScene $ modelRoot </> modelName
  vaoData <- forM meshes $ loadMeshes modelRoot defaultTexture

  let modelRoot' = "res" </> "models"
      modelName' = "golden_icosphere-2.dae"
      defaultTexture' = "res" </> "models" </> "simple-cube-2.bmp"
  (AssImpScene meshes') <- loadAssImpScene $ modelRoot' </> modelName'
  vaoData' <- forM meshes' $ loadMeshes modelRoot' defaultTexture'

  src :: AL.Source <- ON.genObjectName
  sbuf <- AL.createBuffer (AL.File $ "res" </> "sound" </> "africa-toto.wav")
  AL.buffer src AL.$= Just sbuf

  let e1 = Entity
           { _entityChildren = empty
           , _entityGraphics = Just Gfx
             { _gfxVaoData = vaoData
             , _gfxChildren = empty
             }
           , _entitySounds = Just Sfx
             { _sfxSources = singleton src }
           , _entityLogic = Just Lfx
             { _lfxScripts = fromList
               [
                 \model_ -> do
                   entityLocalClosestRayCast model_ (L.V3 0 (-2) 0) $
                     const $ setEntityLinearVelocity model_ (L.V3 0 6 0)
                   return model_
               ]
             }
           , _entityCollisionObject = CollisionObject $ P.toCollisionObject rb
           , _entityRigidBody = Just $ RigidBody rb
           }
      e2 = Entity
           { _entityChildren = empty
           , _entityGraphics = Just Gfx
             { _gfxVaoData = vaoData'
             , _gfxChildren = empty
             }
           , _entitySounds = Just Sfx
             { _sfxSources = empty }
           , _entityLogic = Just Lfx
             { _lfxScripts = fromList
               [
                 -- \model_ -> do
                 --   entityLocalClosestRayCast model_ (L.V3 0 (-2) 0) $
                 --     const $ setEntityLinearVelocity model_ (L.V3 0 6 0)
                 --   return model_
               ]
             }
           , _entityCollisionObject = CollisionObject $ P.toCollisionObject rb'
           , _entityRigidBody = Just $ RigidBody rb'
           }

  return (e1, e2)

setupDynamicBuffers :: IO DynamicBufferBundle
setupDynamicBuffers  = do
  -- Do point lights
  let pointLight = Light
                   { _lightPosition = V4 100 100 0 1
                   , _lightIntensity = 1
                   }
      lightBundle = LightBundle
                         { _lightBundleLights = singleton pointLight
                         , _lightBundleNum = 1
                         }

  cdb  <- genName'
  CameraBlock $= cdb

  plbdb <- genName'
  plbdb ~& FullBufferWrite .$= lightBundle
  LightBlock $= plbdb

  smdb <- genName'
  ShaderMaterialBlock $= smdb

  return DynamicBufferBundle
    { _dynamicBufferBundleShaderCameraBuffer = cdb
    , _dynamicBufferBundleShaderMaterialBuffer = smdb
    , _dynamicBufferBundleLightBundleBuffer = plbdb
    }

gameMain :: IO ()
gameMain = runResourceTChecked $ AL.withProgNameAndArgs AL.runALUT $ \_progName _args -> go
  where
    go :: ResIO ()
    go = do
      ()  <- allocLongR defaultGraphicsContext
      win <- allocLongR defaultWindowConfig
      CurrentContext $= Just win
      --cullFace $= Just Back
      DepthTest $= Just DepthLess
      -- FIXME: we will eventually have to free the function pointer
      -- that mkGLDEBUGPROC gives us!!!
      DebugMessageCallback $= Just simpleDebugFunc

      ClearColor $= color4 0 0 0.4 0
      -- GL.viewport $= (GL.Position 0 0, GL.Size 1920 1080)
      mdev <- AL.openDevice Nothing
      let dev = fromMaybe (error "Couldn't open the sound device.") mdev
      mctxt <- AL.createContext dev []
      let ctxt = fromMaybe (error "Couldn't create the sound context.") mctxt
      AL.currentContext AL.$= Just ctxt
      -- ((pPhong, _, _), (pNormalMap, _, _)) <- compilePipeline

      (pToon, _, _) <- compileToonPipeline

      AL.distanceModel AL.$= AL.InverseDistance
      (physicsWorld, player, cam, _, _) <- liftIO (setupPhysics (pToon, pToon))

      buffBundle <- liftIO setupDynamicBuffers
      specularTex <- load1DPngTexture $ "res" </> "textures" </> "specular1d" ClassyP.<.> "png"

      ic <- liftIO $ N.mkInputControl win
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
                                       & clearBufferStencil .~ True
            entities <- use $ gameStatePhysicsWorld . physicsWorldEntities
            entities' <- mapM animateEntity entities
            gameStatePhysicsWorld . physicsWorldEntities .= entities'

          physicsWire :: GameEffectWire s
          physicsWire = effectWire $ do
            pw <- use gameStatePhysicsWorld
            stepPhysicsWorld pw

          playSchemeWire :: GameEffectWire s
          playSchemeWire = concatA $ fromList
                           [ passWire $ playerHorizontalMovement >>> movePlayer
                           , close
                           , devConsoleToggleWire
                           , jump
                           ]

          devConsoleWire :: GameEffectWire s
          devConsoleWire = concatA $ fromList
                           [ devConsoleToggleWire
                           , devConsoleWriteWire
                           , devConsoleDelWire
                           , executeBufferWire
                           ]

          schemeSelector :: InputScheme -> GameWire s a a
          schemeSelector = \case
            InputPlaying    -> playSchemeWire
            InputDevConsole -> devConsoleWire


          mainWires = fromList [ animationWire
                               , stateSwitchingWire (use gameStateKeyboardInputScheme) schemeSelector
                               , physicsWire
                               , camera
                               , zoomCamera
--                               , turnPlayer
                               ]

          gameState = initGameState & gameStatePhysicsWorld .~ physicsWorld
                                    & gameStatePlayer .~ player
                                    & gameStateCamera .~ cam
                                    & gameStateSoundDevice .~ dev
                                    & gameStateSoundContext .~ ctxt
                                    & gameStateWires .~ mainWires
                                    & gameStateIOData .~ ioData
                                    & gameStateDynamicBufferBundle .~ buffBundle
                                    & gameStateSpecular1DTexture .~ specularTex
      doGame gameState
