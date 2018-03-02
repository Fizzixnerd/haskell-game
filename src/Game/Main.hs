{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Main where

import           ClassyPrelude as ClassyP
import           Control.Lens
import           Control.Wire.Core
import qualified Data.ObjectName as ON
import           FRP.Netwire
import qualified FRP.Netwire.Input.GLFW      as N
import           Game.Types
import           Game.Graphics.Model.ObjLoader
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
import           Game.Wires
import           Game.Graphics.Model.AssImp

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
      flip unlessM go $ use gameStateShouldClose
--      gssc <- use gameStateShouldClose
--      ClassyP.unless gssc go

setupPhysics :: IO (PhysicsWorld s, Player s, Camera s, Entity s, P.RigidBody)
setupPhysics = do
  (theCubeE, theCubeRB) <- createTheCube
  pw <- newPhysicsWorld
  pl' <- newPlayer
  let pl = pl' & playerEntity . entityGraphics .~ (theCubeE ^. entityGraphics)
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
  pw'''' <- addEntityToPhysicsWorld theCubeE pw'''
  setGravityPhysicsWorld (L.V3 0 (-10) 0) pw''''
  P.kccSetGravity (cam ^. cameraController) 0 0 0
  return (pw'''', pl, cam, theCubeE, theCubeRB)

createTheCube :: IO (Entity s, P.RigidBody)
createTheCube = do
  cube <- P.newBoxShape 0.5 0.5 0.5
  startXform <- P.new ((0, 0, 0, 0), (0, 0, 0))
  P.setIdentity startXform
  P.setOrigin startXform 0 1 0
  -- Don't delete the ms!!  See below.
  ms <- P.new startXform
  rbci <- P.newRigidBodyConstructionInfo 1 ms cube 0 1 0
  -- The MotionState ms now belongs to the cube.
  rb   <- P.newRigidBody rbci
  P.del startXform
  P.del rbci

  prog <- compileShaders
  modelVec <- loadAssImpMeshes2D "res/models/Bayonetta 1/bayo_default.dae"
  -- [(vao, _, ebuf)]
  vaos <- forM modelVec $ \(objPoints, objIndices) -> do
    --  (objPoints', objIndices') <- loadObjVTN "res/models/simple-cube-2.obj"

    let posLocation = AttribLocation 0
        texLocation = AttribLocation 1
        nmlLocation = AttribLocation 2
    bufferDataAssImp posLocation texLocation nmlLocation objPoints objIndices

  tex <- loadBMPTexture "res/models/simple-cube-2.bmp"
  src :: AL.Source <- ON.genObjectName
  sbuf <- AL.createBuffer (AL.File "res/sound/africa-toto.wav")
  AL.buffer src $= Just sbuf

  let e = Entity
          { _entityChildren = empty
          , _entityGraphics = Just Gfx
            { _gfxVaoData = (\(vao, _, ebuf) -> (vao, prog, Triangles, fromIntegral $ snd ebuf)) <$> vaos
            , _gfxTextureBlob = GfxTexture () (Just (Simple2DSampler, tex)) ()
            , _gfxChildren = empty
            }
          , _entitySounds = Just Sfx
            { _sfxSources = singleton src }
          , _entityLogic = Just Lfx
            { _lfxScripts = fromList
              [ \cube_ -> return cube_
              , \cube_ -> do
                  entityLocalClosestRayCast cube_ (L.V3 0 (-2) 0) $ 
                    \_ -> do
                      setEntityLinearVelocity cube_ (L.V3 0 6 0)
                  return cube_
              ]
            }
          , _entityCollisionObject = CollisionObject $ P.toCollisionObject rb
          , _entityRigidBody = Just $ RigidBody rb
          }
  return (e, rb)

concatA :: ArrowPlus a => Vector (a b b) -> a b b
concatA = foldr (<+>) id

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
    (physicsWorld, player, cam, _, _) <- setupPhysics

    -- no need because we already add the entity.
    -- physicsWorld <- addRigidBodyToPhysicsWorld theCubeRB physicsWorld'
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
                             , (playerHorizontalMovement >>> movePlayer)
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
    doGame gameState
