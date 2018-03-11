{-# LANGUAGE OverloadedStrings #-}
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
import           Control.Lens
import qualified Control.Monad.Catch          as MC
import           Control.Monad.Fix            as Fix
import qualified Control.Monad.Logger         as ML
import qualified Control.Monad.State.Strict   as MSS
import qualified Control.Monad.Trans.Resource as RT
import           Data.Dynamic
import qualified Data.Map.Strict              as MS
import           FRP.Netwire
import qualified FRP.Netwire.Input.GLFW       as N
import           Foreign.C.Types
import           Graphics.Binding
import qualified Physics.Bullet               as P
import qualified Sound.OpenAL                 as AL
import           Text.Printf
import           Foreign.Resource
import           Game.Graphics.Types

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

runGame :: GameState s -> Game s a -> ResIO (a, GameState s)
runGame gs g = ML.runStderrLoggingT . MSS.runStateT (_unGame g) $ gs

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
  , _gameStateDynamicBufferBundle :: DynamicBufferBundle
  , _gameStateSpecular1DTexture   :: TextureObject TextureTarget1D
  , _gameStateKeyboardInputScheme :: InputScheme
  , _gameStateDevConsole          :: Maybe DevConsole
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
  , _gameStateDynamicBufferBundle  = error "bufferBundle not set."
  , _gameStateSpecular1DTexture    = error "specular1DTexture not set."
  , _gameStateKeyboardInputScheme  = InputPlaying
  , _gameStateDevConsole           = Nothing
  }

data Camera s = Camera
  { _cameraFOV :: Float
  , _cameraController        :: P.RigidBody
  , _cameraTarget            :: P.CollisionObject
  , _cameraPreferredDistance :: CFloat
  , _cameraEntity            :: Entity s
  }

data GiantFeaturelessPlane s = GiantFeaturelessPlane
  { _giantFeaturelessPlaneRigidBody :: P.RigidBody
  , _giantFeaturelessPlaneEntity :: Entity s
  }

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

data Entity s = Entity
  { _entityGraphics        :: Maybe (Gfx s)
  , _entitySounds          :: Maybe (Sfx s)
  , _entityLogic           :: Maybe (Lfx s)
  , _entityChildren        :: Vector (Entity s)
  , _entityCollisionObject :: CollisionObject
  , _entityRigidBody       :: Maybe RigidBody
  }

data Player s = Player
  { _playerController :: P.RigidBody
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

data VaoData = VaoData
  { _vaoDataVao            :: VertexArrayObject
  , _vaoDataShaderPipeline :: ShaderPipeline
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

newtype Sfx s = Sfx
  { _sfxSources :: Vector AL.Source
  }

newtype Lfx s = Lfx
  { _lfxScripts :: Vector (Entity s -> Game s (Entity s))
  }

data InputScheme
  = InputPlaying
  | InputDevConsole
  deriving (Eq, Ord, Show, Enum, Bounded)

mconcat <$> mapM makeLenses
  [ ''Camera
  , ''Entity
  , ''EventRegister
  , ''Game
  , ''GameState
  , ''Gfx
  , ''GiantFeaturelessPlane
  , ''Lfx
  , ''CollisionObject
  , ''PhysicsWorld
  , ''Player
  , ''RigidBody
  , ''Script
  , ''ScriptName
  , ''Sfx
  , ''IOData
  , ''VaoData
  ]
