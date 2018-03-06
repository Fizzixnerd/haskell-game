{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.Types where

import           ClassyPrelude
import qualified Codec.Wavefront              as W
import           Control.Lens
import qualified Control.Monad.Catch          as MC
import           Control.Monad.Fix            as Fix
import qualified Control.Monad.Logger         as ML
import qualified Control.Monad.State.Strict   as MSS
import qualified Control.Monad.Trans.Resource as RT
import           Data.Dynamic
import qualified Data.Map.Strict              as MS
import qualified Data.Vector.Storable         as VS
import           FRP.Netwire
import qualified FRP.Netwire.Input.GLFW       as N
import           Foreign.C.Types
import           Foreign.Storable
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Graphics.Binding
import qualified Linear                       as L
import qualified Physics.Bullet               as P
import qualified Sound.OpenAL                 as AL
import           Text.Printf

type GameWire s a b = Wire s () (Game s) a b
type GameEffectWire s = forall a. GameWire s a a

newtype Game s a = Game
  { _unGame :: MSS.StateT (GameState s) (ML.LoggingT RT.ResIO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MSS.MonadState (GameState s)
             , MC.MonadThrow
             , MC.MonadCatch
             , MC.MonadMask
             , Fix.MonadFix
             , RT.MonadResource
             , MonadBase IO
             , MonadBaseControl IO
             )

instance ML.MonadLogger (Game s) where
  monadLoggerLog l ls ll m = Game $ lift $ ML.monadLoggerLog l ls ll m

instance Fix.MonadFix m => Fix.MonadFix (ML.LoggingT m) where
  mfix f = ML.LoggingT $ \r -> Fix.mfix $ \a -> ML.runLoggingT (f a) r

instance N.MonadGLFWInput (Game s) where
  getGLFWInput     = MSS.gets $ _ioDataGLFWInputState . _gameStateIOData
  putGLFWInput inp = MSS.modify' $ \s ->
    let
      iod = (_gameStateIOData s) { _ioDataGLFWInputState = inp }
    in
      s { _gameStateIOData = iod }

data IOData = IOData
  { _ioDataGLFWInputControl :: N.GLFWInputControl
  , _ioDataGLFWInputState   :: N.GLFWInputState
  , _ioDataSession          :: Session IO (Timed Integer ())
  , _ioDataWindow           :: Window
  }

initIOData :: IOData
initIOData = IOData (error "No GLFWInputControl") (error "No GLFWInputState") (error "No wire session") (error "No window")

runGame :: GameState s -> Game s a -> IO (a, GameState s)
runGame gs g = RT.runResourceTChecked . ML.runStderrLoggingT . MSS.runStateT (_unGame g) $ gs

releaseResources :: Game s a -> Game s a
releaseResources act = do
  s <- MSS.get
  (a, s') <- liftIO $ runGame s act
  MSS.put s'
  return a

newtype EventRegister s = EventRegister
  { _unEventRegister :: MS.Map EventName (GameWire s () (Event Dynamic))
  }

type ScanCode = Int

data GameState s = GameState
  { _gameStateIOData                 :: IOData
  , _gameStateCamera                 :: Camera s
  , _gameStateActiveScripts          :: Vector (Script s)
  , _gameStateEventRegister          :: EventRegister s
  , _gameStateKeyEvent               :: GameWire s () (Event Key)
  , _gameStateMousePosEvent          :: GameWire s () (Event (Double, Double))
  , _gameStatePhysicsWorld           :: PhysicsWorld s
  , _gameStatePlayer                 :: Player s
  , _gameStateMouseSpeed             :: Float
  , _gameStateShouldClose            :: Bool
  , _gameStateSoundContext           :: AL.Context
  , _gameStateSoundDevice            :: AL.Device
  , _gameStateWires                  :: Vector (GameWire s () ())
  , _gameStatePersistentBufferBundle :: PersistentBufferBundle
  }

initGameState :: GameState s
initGameState = GameState
  { _gameStateIOData                  = error "ioData not set"
  , _gameStateCamera                  = error "camera not set."
  , _gameStateActiveScripts           = empty
  , _gameStateEventRegister           = EventRegister mempty
  , _gameStateMousePosEvent           = error "mousePosEvent not set."
  , _gameStateKeyEvent                = error "keyEvent not set."
  , _gameStatePhysicsWorld            = error "physicsWorld not set."
  , _gameStatePlayer                  = error "player not set."
  , _gameStateMouseSpeed              = 0.01
  , _gameStateShouldClose             = False
  , _gameStateSoundContext            = error "soundContext not set."
  , _gameStateSoundDevice             = error "soundDevice not set."
  , _gameStateWires                   = empty
  , _gameStatePersistentBufferBundle  = error "bufferBundle not set."
  }

data Camera s = Camera
  { _cameraFOV :: Float
  , _cameraController        :: P.KinematicCharacterController
  , _cameraTarget            :: P.CollisionObject
  , _cameraPreferredDistance :: CFloat
  , _cameraEntity            :: Entity s
  }

data GiantFeaturelessPlane s = GiantFeaturelessPlane
  { _giantFeaturelessPlaneRigidBody :: P.RigidBody
  , _giantFeaturelessPlaneEntity :: Entity s }

data ExpandObjVTN = ExpandObjVTN
  { _expandObjVTNIndMap :: MS.Map VTNIndex CUInt
  , _expandObjVTNPoints :: [VTNPoint]
  , _expandObjVTNIndices :: [CUInt]
  , _expandObjVTNNextInd :: CUInt
  , _expandObjVTNVerts :: Vector W.Location
  , _expandObjVTNTexs :: Vector W.TexCoord
  , _expandObjVTNNorms :: Vector W.Normal
  } deriving (Eq, Show)

type ModuleName = String
type EventName = Text
type EndoName = Text

data ScriptName = ScriptName
  { _scriptNamePath :: FilePath
  , _scriptNameMainModule :: ModuleName
  } deriving (Eq, Ord, Show)

data Script s = Script
  { _scriptSuperScripts :: Vector ScriptName
  , _scriptName :: ScriptName
  , _scriptOnInit :: Game s ()
  , _scriptOnLoad :: Game s ()
  , _scriptOnEvent :: Vector (EventName, Dynamic -> Game s ())
  , _scriptOnUnload :: Game s ()
  , _scriptOnExit :: Game s ()
  }

instance Eq (Script s) where
  Script { _scriptName = sn1 } == Script { _scriptName = sn2 } = sn1 == sn2

instance Ord (Script s) where
  compare Script { _scriptName = sn1 } Script { _scriptName = sn2 } = compare sn1 sn2

instance Show (Script s) where
  show Script {..} = printf "<<Script \"%s\">>" (show _scriptName)

defaultScript :: Script s
defaultScript = Script
  { _scriptSuperScripts = empty
  , _scriptName = error "Don't change this."
  , _scriptOnInit = return ()
  , _scriptOnLoad = return ()
  , _scriptOnEvent = empty
  , _scriptOnUnload = return ()
  , _scriptOnExit = return ()
  }

lookupEventByName :: EventName
                  -> EventRegister s
                  -> Maybe (GameWire s () (Event Dynamic))
lookupEventByName en (EventRegister er) = lookup en er

registerEventByName :: EventName
                    -> GameWire s () (Event Dynamic)
                    -> EventRegister s
                    -> Map EventName (GameWire s () (Event Dynamic))
registerEventByName en e (EventRegister er) = MS.insert en e er

-- You can't really deregister events...
-- deregisterEventByName :: EventName -> EventRegister -> EventRegister
-- deregisterEventByName en (EventRegister er) = EventRegister $ MS.delete en er

-- registerEndoByName :: EventName
--                    -> EndoName
--                    -> (GameState -> Game GameState)
--                    -> EventRegister
--                    -> EndoRegister
--                    -> EndoRegister
-- registerEndoByName eventName endoName endo eventR endoR =
--   let me = lookupEventByName eventName eventR in
--   case me of
--     Nothing -> endoR
--     Just e -> do
--       let e' = const endo <$> e
--       registerEndo endoName e' endoR

-- registerEndo :: EndoName -> B.Event (GameState -> B.MomentIO GameState) -> EndoRegister -> EndoRegister
-- registerEndo en e (EndoRegister er) = EndoRegister $ MS.insert en e er

data Player s = Player
  { _playerController :: P.KinematicCharacterController
  , _playerEntity     :: Entity s
  }

data PhysicsWorld s = PhysicsWorld
  { _physicsWorldDynamicsWorld :: P.DynamicsWorld
  , _physicsWorldPlayers :: Vector (Player s)
  , _physicsWorldGiantFeaturelessPlanes :: Vector (GiantFeaturelessPlane s)
  , _physicsWorldCameras :: Vector (Camera s)
  , _physicsWorldBroadphaseInterface :: P.BroadphaseInterface
  , _physicsWorldGhostPairCallback :: P.GhostPairCallback
  , _physicsWorldCollisionConfiguration :: P.CollisionConfiguration
  , _physicsWorldCollisionDispatcher :: P.CollisionDispatcher
  , _physicsWorldConstraintSolver :: P.ConstraintSolver
  , _physicsWorldCollisionObjects :: Vector P.CollisionObject
  , _physicsWorldRigidBodies :: Vector P.RigidBody
  , _physicsWorldEntities :: Vector (Entity s)
  }

data VTNPoint = VTNPoint
  { _vtnPointV :: !(L.V4 CFloat)
  , _vtnPointT :: !(L.V2 CFloat)
  , _vtnPointN :: !(L.V3 CFloat)
  } deriving (Eq, Show, Ord, Read)

data AssImpVertex = AssImpVertex
  { _assImpVertexV :: !(L.V3 Float)
  , _assImpVertexT :: !(L.V2 Float)
  , _assImpVertexN :: !(L.V3 Float)
  } deriving (Eq, Show, Ord, Read)

data VTNIndex = VTNIndex
  { _vtnIndexV :: !Int
  , _vtnIndexT :: !Int
  , _vtnIndexN :: !Int
  } deriving (Eq, Show, Ord, Read)

instance Storable VTNPoint where
  sizeOf _ = 9 * sizeOf (0 :: CFloat)
  alignment _ = alignment (0 :: CFloat)
  poke ptr (VTNPoint v t n) = do
    pokeByteOff ptr 0 v
    pokeByteOff ptr (4 * sizeOf (0 :: CFloat)) t
    pokeByteOff ptr (6 * sizeOf (0 :: CFloat)) n
  peek ptr = do
    v <- peekByteOff ptr 0
    t <- peekByteOff ptr (4 * sizeOf (0 :: CFloat))
    n <- peekByteOff ptr (6 * sizeOf (0 :: CFloat))
    return $ VTNPoint v t n

instance Storable AssImpVertex where
  sizeOf _ = 8 * sizeOf (0 :: Float)
  alignment _ = alignment (0 :: Float)
  poke ptr (AssImpVertex v t n) = do
    pokeByteOff ptr 0 v
    pokeByteOff ptr (3 * sizeOf (0 :: Float)) t
    pokeByteOff ptr (5 * sizeOf (0 :: Float)) n
  peek ptr = do
    v <- peekByteOff ptr 0
    t <- peekByteOff ptr (3 * sizeOf (0 :: Float))
    n <- peekByteOff ptr (5 * sizeOf (0 :: Float))
    return $ AssImpVertex v t n

instance Storable VTNIndex where
  sizeOf _ = 3 * sizeOf (0 :: Int)
  alignment _ = alignment (0 :: Int)
  poke ptr (VTNIndex v t n) = do
    pokeByteOff ptr 0 v
    pokeByteOff ptr (1 * sizeOf (0 :: Int)) t
    pokeByteOff ptr (2 * sizeOf (0 :: Int)) n
  peek ptr = do
    v <- peekByteOff ptr 0
    t <- peekByteOff ptr (1 * sizeOf (0 :: Int))
    n <- peekByteOff ptr (2 * sizeOf (0 :: Int))
    return $ VTNIndex v t n

type WindowWidth  = Int
type WindowHeight = Int

data MousePos = MousePos
  { _mousePos :: !(L.V2 Double)
  } deriving (Eq, Ord, Show)

data Entity s = Entity
  { _entityGraphics        :: Maybe (Gfx s)
  , _entitySounds          :: Maybe (Sfx s)
  , _entityLogic           :: Maybe (Lfx s)
  , _entityChildren        :: Vector (Entity s)
  , _entityCollisionObject :: CollisionObject
  , _entityRigidBody       :: Maybe RigidBody
  }

data TextureBundle s = TextureBundle
  { _textureBundleDiffuseTexture      :: Maybe s
  , _textureBundleSpecularTexture     :: Maybe s
  , _textureBundleAmbientTexture      :: Maybe s
  , _textureBundleEmmisiveTexture     :: Maybe s
  , _textureBundleHeightTexture       :: Maybe s
  , _textureBundleNormalTexture       :: Maybe s
  , _textureBundleShininessTexture    :: Maybe s
  , _textureBundleOpacityTexture      :: Maybe s
  , _textureBundleDisplacementTexture :: Maybe s
  , _textureBundleLightMapTexture     :: Maybe s
  , _textureBundleReflectionTexture   :: Maybe s
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

emptyTextureBundle :: TextureBundle s
emptyTextureBundle = TextureBundle Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data PersistentBufferBundle = PersistentBufferBundle
  { _persistentBufferBundleShaderMaterialBuffer :: PersistentBuffer ShaderMaterial
  , _persistentBufferBundleShaderCameraBuffer :: PersistentBuffer ShaderCamera
  , _persistentBufferBundlePointLightBundleBuffer :: PersistentBuffer PointLightBundle
  } deriving (Eq, Ord, Show)

data VaoData = VaoData
  { _vaoDataVao            :: VertexArrayObject
  , _vaoDataProgram        :: Program
  , _vaoDataPrimitiveMode  :: PrimitiveMode
  , _vaoDataNumElements    :: Word32
  , _vaoDataTextureBundle  :: TextureBundle (TextureObject TextureTarget2D)
  , _vaoDataShaderMaterial :: ShaderMaterial
  } deriving (Eq, Ord, Show)

data Gfx s = Gfx
  { _gfxVaoData  :: Vector VaoData
  , _gfxChildren :: Vector (Gfx s)
  } deriving (Eq, Ord, Show)

newtype CollisionObject = CollisionObject { _unCollisionObject :: P.CollisionObject }
newtype RigidBody       = RigidBody       { _unRigidBody       :: P.RigidBody       }

type VPMatrix = L.M44 Float
type VMatrix  = L.M44 Float
type PMatrix  = L.M44 Float

data Sfx s = Sfx
  { _sfxSources :: Vector AL.Source
  }

data Lfx s = Lfx
  { _lfxScripts :: Vector (Entity s -> Game s (Entity s))
  }

newtype AssImpScene = AssImpScene
  { _assImpMeshes :: Vector AssImpMesh }

data AssImpMesh = AssImpMesh
  { _assImpMeshVAO            :: VertexArrayObject
  , _assImpMeshBufferObject   :: BufferObject
  , _assImpMeshTextureDetails :: Vector Word32
  , _assImpMeshIndexBO        :: BufferObject
  , _assImpMeshIndexBOType    :: IndexType
  , _assImpMeshIndexNum       :: Word32
  , _assImpMeshTextureBundle  :: TextureBundle FilePath
  , _assImpMeshShaderMaterial :: ShaderMaterial
  }

data PointLight = PointLight
  { _pointLightPosition :: L.V4 Float
  , _pointLightIntensity :: Float
  } deriving (Eq, Ord, Show)

instance Storable PointLight where
  sizeOf _ = (1 + 4) * sizeOf (0 :: Float)
  alignment _ = 4 * alignment (error "unreachable":: Float)
  poke ptr (PointLight loc str) = do
    pokeByteOff ptr 0 loc
    pokeByteOff ptr (4 * sizeOf (0 :: Float)) str
  peek ptr = do
    loc <- peekByteOff ptr 0
    str <- peekByteOff ptr (4 * sizeOf (0 :: Float))
    return $ PointLight loc str

data PointLightBundle = PointLightBundle
  { _pointLightBundleLights :: VS.Vector PointLight
  , _pointLightBundleNum :: Int
  }

data ShaderCamera = ShaderCamera
  { _shaderCameraMVP :: L.M44 Float
  , _shaderCameraVP  :: L.M44 Float
  , _shaderCameraP   :: L.M44 Float
  } deriving (Eq, Ord, Show)

instance Storable ShaderCamera where
  sizeOf _ = 3 * sizeOf (error "unreachable" :: L.M44 Float)
  alignment _ = 4 * sizeOf (error "unreachable" :: L.M44 Float)
  poke ptr (ShaderCamera mvp vp p) = do
    pokeElemOff (castPtr ptr) 0 mvp
    pokeElemOff (castPtr ptr) 1 vp
    pokeElemOff (castPtr ptr) 2 p
  peek = error "ShaderCamera: WHY YOU TRY TO PEEK!? B-BAKA!"

data CameraBlock = CameraBlock deriving (Eq, Ord, Show)

instance Uniform CameraBlock where
  type UniformContents CameraBlock = PersistentBuffer ShaderCamera
  type UniformLocationType CameraBlock = DefaultBlock
  uniform prg _ _ = uniformBlockBinding prg 0 0

instance UniformBlock CameraBlock (PersistentBuffer ShaderCamera) where
  bindBlock_ _ = bindFullPersistentBufferToPoint 0

maxPointLights :: Int
maxPointLights = 128

instance Storable PointLightBundle where
  sizeOf _ = maxPointLights * 5 * sizeOf (0 :: Float) + sizeOf (0 :: Int)
  alignment = error "Please don't use this."
  poke ptr (PointLightBundle ls n) = VS.unsafeWith ls $ \lsPtr -> do
    copyBytes ptr (castPtr lsPtr) $ min (maxPointLights * 5 * sizeOf (0 :: Float)) (length ls * sizeOf (0 :: Float) * 5)
    poke (castPtr (ptr `plusPtr` (maxPointLights * 4 * sizeOf (0 :: Float)))) n
  peek = error "PointLightBundle: WHY YOU TRY TO PEEK!? B-BAKA!"

data PointLightBlock = PointLightBlock deriving (Eq, Ord, Show)

instance Uniform PointLightBlock where
  type UniformContents PointLightBlock = PersistentBuffer PointLightBundle
  type UniformLocationType PointLightBlock = DefaultBlock
  uniform prg _ _ = uniformBlockBinding prg 1 1

instance UniformBlock PointLightBlock (PersistentBuffer PointLightBundle) where
  bindBlock_ _ = bindFullPersistentBufferToPoint 1

data ShaderMaterial = ShaderMaterial
  { _shaderMaterialDiffuseColor     :: L.V4 Float
  , _shaderMaterialAmbientColor     :: L.V4 Float
  , _shaderMaterialSpecularColor    :: L.V4 Float
  , _shaderMaterialSpecularStrength :: Float
  , _shaderMaterialSpecularExponent :: Float
  } deriving (Eq, Ord, Show)

instance Storable ShaderMaterial where
  sizeOf _ = 11 * sizeOf (error "unreachable" :: Float)
  alignment _ = 4 * alignment (error "unreachable" :: Float)
  poke ptr ShaderMaterial {..} = do
    pokeByteOff (castPtr ptr) (0*m) _shaderMaterialDiffuseColor
    pokeByteOff (castPtr ptr) (4*m) _shaderMaterialAmbientColor
    pokeByteOff (castPtr ptr) (8*m) _shaderMaterialSpecularColor
    pokeByteOff (castPtr ptr) (12*m) _shaderMaterialSpecularStrength
    pokeByteOff (castPtr ptr) (16*m) _shaderMaterialSpecularExponent
    where
      m = sizeOf (error "unreachable" :: Float)
  peek = error "ShaderMaterial: WHY YOU TRY TO PEEK!? B-BAKA!"

data ShaderMaterialBlock = ShaderMaterialBlock deriving (Eq, Ord, Show)

instance Uniform ShaderMaterialBlock where
  type UniformContents ShaderMaterialBlock = PersistentBuffer ShaderMaterial
  type UniformLocationType ShaderMaterialBlock = DefaultBlock
  uniform prg _ _ = uniformBlockBinding prg 2 2

instance UniformBlock ShaderMaterialBlock (PersistentBuffer ShaderMaterial) where
  bindBlock_ _ = bindFullPersistentBufferToPoint 2

mconcat <$> mapM makeLenses
  [ ''Camera
  , ''Entity
  , ''EventRegister
  , ''ExpandObjVTN
  , ''Game
  , ''GameState
  , ''Gfx
  , ''GiantFeaturelessPlane
  , ''Lfx
  , ''CollisionObject
  , ''MousePos
  , ''PhysicsWorld
  , ''Player
  , ''RigidBody
  , ''Script
  , ''ScriptName
  , ''Sfx
  , ''VTNIndex
  , ''VTNPoint
  , ''IOData
  , ''AssImpVertex
  , ''AssImpScene
  , ''AssImpMesh
  , ''VaoData
  , ''TextureBundle
  , ''PointLight
  , ''PointLightBundle
  , ''PersistentBufferBundle
  ]
