{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.Types where

import           ClassyPrelude
import qualified Codec.Wavefront             as W
import           Control.Lens
import           Control.Lens.Action
import qualified Control.Monad.Logger        as ML
import qualified Control.Monad.State.Strict  as MSS
import qualified Control.Monad.Catch         as MC
import qualified Data.Map.Strict             as MS
import           Foreign.C.Types
import           Foreign.Storable
import           FRP.Netwire
import qualified FRP.Netwire.Input.GLFW      as N
import           Graphics.Binding
import qualified Linear                      as L
import qualified Physics.Bullet              as P
import           Text.Printf
import           Data.Dynamic
import           Control.Monad.Fix           as Fix

type GameWire s a b = Wire s () (Game s) a b

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
  , _gameStateMouseSpeed    :: Float
  , _gameStateShouldClose   :: Bool
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
  }

data Camera = Camera
  { _cameraPosition :: L.V3 Float
  , _cameraOrientation :: (Float, Float)
  , _cameraFOV :: Float
  , _cameraController     :: P.CollisionObject
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
  { _playerPhysicsController :: P.KinematicCharacterController
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

type MSetter s a = forall m. MonadIO m => Setter s (m s) a (m a)
type MGetter s a = forall m. MonadIO m => Action m s a

mconcat <$> mapM makeLenses
  [ ''Camera
  , ''GameState
  , ''ExpandObjVTN
  , ''Script
  , ''ScriptName
  , ''EventRegister
  , ''Player
  , ''PhysicsWorld
  , ''GiantFeaturelessPlane
  , ''VTNPoint
  , ''VTNIndex
  , ''MousePos
  , ''Game
  ]
