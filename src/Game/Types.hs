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
import qualified Codec.Wavefront             as W
import           Control.Lens
import qualified Control.Monad.Logger        as ML
import qualified Control.Monad.State.Strict  as MSS
import qualified Control.Monad.Catch         as MC
import qualified Data.Map.Strict             as MS
import           Foreign.C.Types
import           Foreign.Storable
import           FRP.Netwire
import qualified FRP.Netwire.Input.GLFW      as N
import           Graphics.Binding
import qualified Sound.OpenAL                as AL
import           Game.Graphics.Texture.Loader
import qualified Linear                      as L
import qualified Physics.Bullet              as P
import           Text.Printf
import           Data.Dynamic
import           Control.Monad.Fix           as Fix

type GameWire s a b = Wire s () (Game s) a b
type GameEffectWire s a = GameWire s a a

newtype Game s a = Game
  { _unGame :: N.GLFWInputT (MSS.StateT (GameState s) (ML.LoggingT IO)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MSS.MonadState (GameState s)
             , MC.MonadThrow
             , MC.MonadCatch
             , MC.MonadMask
             , N.MonadGLFWInput
             , Fix.MonadFix )

instance ML.MonadLogger (Game s) where
  monadLoggerLog l ls ll m = Game $ lift $ ML.monadLoggerLog l ls ll m

instance Fix.MonadFix m => Fix.MonadFix (ML.LoggingT m) where
  mfix f = ML.LoggingT $ \r -> Fix.mfix $ \a -> ML.runLoggingT (f a) r

runGame :: GameState s -> N.GLFWInputControl -> Game s a -> IO ((a, N.GLFWInputState), GameState s)
runGame s ic g = do
  input <- N.getInput ic
  ML.runStderrLoggingT $ MSS.runStateT (N.runGLFWInputT (_unGame g) input) s

newtype EventRegister s = EventRegister
  { _unEventRegister :: MS.Map EventName (GameWire s () (Event Dynamic))
  }

type ScanCode = Int

data GameState s = GameState
  { _gameStateCamera        :: Camera
  , _gameStateActiveScripts :: Vector (Script s)
  , _gameStateEventRegister :: EventRegister s
  , _gameStateKeyEvent      :: GameWire s () (Event Key)
  , _gameStateMousePosEvent :: GameWire s () (Event (Double, Double))
  , _gameStatePhysicsWorld  :: PhysicsWorld
  , _gameStatePlayer        :: Player
  , _gameStateEntities      :: Vector (Entity s)
  , _gameStateMouseSpeed    :: Float
  , _gameStateShouldClose   :: Bool
  , _gameStateSoundContext  :: AL.Context
  , _gameStateSoundDevice   :: AL.Device
  }

initGameState :: GameState s
initGameState = GameState
  { _gameStateCamera = error "camera not set."
  , _gameStateActiveScripts = empty
  , _gameStateEventRegister = EventRegister mempty
  , _gameStateMousePosEvent = error "mousePosEvent not set."
  , _gameStateKeyEvent      = error "keyEvent not set."
  , _gameStatePhysicsWorld  = error "physicsWorld not set."
  , _gameStatePlayer        = error "player not set."
  , _gameStateMouseSpeed    = 0.01
  , _gameStateShouldClose   = False
  , _gameStateEntities      = empty
  , _gameStateSoundContext  = error "soundContext not set."
  , _gameStateSoundDevice   = error "soundDevice not set."
  }

data Camera = Camera
  { _cameraFOV :: Float
  , _cameraController     :: P.KinematicCharacterController
  , _cameraTarget         :: P.CollisionObject
  , _cameraPreferredDistance :: CFloat
  }

newtype GiantFeaturelessPlane = GiantFeaturelessPlane { _unGiantFeauturelessPlane :: P.RigidBody }

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

data Player = Player
  { _playerController :: P.KinematicCharacterController
  }

data PhysicsWorld = PhysicsWorld
  { _physicsWorldDynamicsWorld :: P.DynamicsWorld
  , _physicsWorldPlayers :: Vector Player
  , _physicsWorldGiantFeaturelessPlanes :: Vector GiantFeaturelessPlane
  , _physicsWorldCameras :: Vector Camera
  , _physicsWorldBroadphaseInterface :: P.BroadphaseInterface
  , _physicsWorldGhostPairCallback :: P.GhostPairCallback
  , _physicsWorldCollisionConfiguration :: P.CollisionConfiguration
  , _physicsWorldCollisionDispatcher :: P.CollisionDispatcher
  , _physicsWorldConstraintSolver :: P.ConstraintSolver
  , _physicsWorldCollisionObjects :: Vector P.CollisionObject
  , _physicsWorldRigidBodies :: Vector P.RigidBody
  }

data VTNPoint = VTNPoint
  { _vtnPointV :: !(L.V4 CFloat)
  , _vtnPointT :: !(L.V2 CFloat)
  , _vtnPointN :: !(L.V3 CFloat)
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
  { _entityGraphics       :: Maybe (Gfx s)
  , _entitySounds         :: Maybe (Sfx s)
  , _entityLogic          :: Maybe (Lfx s)
  , _entityWorldTransform :: WorldTransform
  }

-- | When an `Entity' is loaded, it's graphics data is stored here.
-- Note that `gfxWire' is constructed from a `GameWire s (Gfx s) ()'.
-- It is then combined with `arr (const <this Gfx s>)' to create the
-- final `Wire' seen below, which is more flexible.  This means the
-- `Wire' actually has speedy access to the `Gfx' object.

data GfxTexture = GfxTexture
  { _gfxTexture1D  :: ()
  , _gfxTexture2D  :: Maybe (Simple2DSampler, TextureObject TextureTarget2D)
  , _gfxTexture3D  :: ()
  } deriving (Eq, Ord, Show)

data Gfx s = Gfx
  { _gfxVaoData     :: Vector (VertexArrayObject, Program, PrimitiveMode, Word32)
  , _gfxTextureBlob :: GfxTexture
  , _gfxChildren    :: Vector (Gfx s)
  }

newtype WorldTransform = WorldTransform { _unWorldTransform :: P.CollisionObject }

type VPMatrix = L.M44 Float
type VMatrix  = L.M44 Float

data Sfx s = Sfx
  { _sfxSources :: Vector AL.Source
  }

data Lfx s = Lfx
  { _lfxWires :: GameWire s (Lfx s) ()
  }

mconcat <$> mapM makeLenses
  [ ''Camera
  , ''Entity
  , ''EventRegister
  , ''ExpandObjVTN
  , ''Game
  , ''GameState
  , ''GfxTexture
  , ''Gfx
  , ''GiantFeaturelessPlane
  , ''Lfx
  , ''WorldTransform
  , ''MousePos
  , ''PhysicsWorld
  , ''Player
  , ''Script
  , ''ScriptName
  , ''Sfx
  , ''VTNIndex
  , ''VTNPoint
  ]
