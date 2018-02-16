{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Game.Types where

import           ClassyPrelude
import qualified Codec.Wavefront             as W
import           Control.Lens
import qualified Control.Monad.Logger        as ML
import qualified Data.Map.Strict             as MS
import           Foreign.C.Types
import           Foreign.Storable
import qualified Graphics.UI.GLFW            as G
import qualified Linear                      as L
import qualified Physics.Bullet              as P
import qualified Reactive.Banana.Combinators as B
import qualified Reactive.Banana.Frameworks  as B
import           Text.Printf

newtype Game a = Game
  { _unGame :: ML.LoggingT IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , ML.MonadLogger
             , MonadIO
             )

runGame :: Game a -> IO a
runGame g = ML.runStderrLoggingT $ _unGame g

newtype EventRegister = EventRegister
  { _unEventRegister :: MS.Map EventName (B.Event ())
  }
newtype EndoRegister  = EndoRegister
  { _unEndoRegister :: MS.Map EndoName (B.Event (GameState -> B.MomentIO GameState))
  }

type ScanCode = Int

data GameState = GameState
  { _gameStateCamera        :: Camera
  , _gameStateActiveScripts :: Vector Script
  , _gameStateEventRegister :: EventRegister
  , _gameStateEndoRegister  :: EndoRegister
  , _gameStateKeyEvent      :: B.Event ( G.Window
                                       , G.Key
                                       , ScanCode
                                       , G.KeyState
                                       , G.ModifierKeys
                                       )
  , _gameStatePhysicsWorld  :: PhysicsWorld
  , _gameStatePlayer        :: Player
  , _gameStateMouseSpeed    :: Float
  , _gameStateMousePos      :: MousePos
  , _gameStateCurrentTime   :: Double
  }

initGameState :: GameState
initGameState = GameState
  { _gameStateCamera = error "camera not set."
  , _gameStateActiveScripts = empty
  , _gameStateEventRegister = EventRegister mempty
  , _gameStateEndoRegister  = EndoRegister mempty
  , _gameStateKeyEvent      = error "keyEvent not set."
  , _gameStatePhysicsWorld  = error "physicsWorld not set."
  , _gameStatePlayer        = error "player not set."
  , _gameStateMouseSpeed    = 0.01
  , _gameStateMousePos      = MousePos (L.V2 0 0)
  , _gameStateCurrentTime   = 0
  }

-- | Camera exists in physics world to deal with collisions.  It also
--   always looks at a target.
data Camera = Camera
  { _cameraController     :: P.CollisionObject
  , _cameraTarget         :: P.CollisionObject
  , _cameraTargetDistance :: CFloat
  }

-- cameraMVP :: Getter Camera (L.M44 Float)
-- cameraMVP = to go
--   where
--     go (Camera vpos (vangh, vangv) cfov) = camPerspective L.!*! camView L.!*! camModel
--       where
--         vup  = L.V3 0 1 0
--         vdir = L.rotate (L.axisAngle (L.V3 0 1 0) vangh * L.axisAngle (L.V3 1 0 0) vangv) (L.V3 0 0 (negate 1))
--         camModel = L.identity
--         camView = L.lookAt vpos (vpos + vdir) vup
--       -- Projection matrix : 90deg Field of View, 16:9 ratio, display range : 0.1 unit <-> 100 units
--         camPerspective = L.perspective cfov (16/9) 0.1 100

data NamedHandler a = NamedHandler
  { _namedHandlerName :: EventName
  , _namedHandlerHandler :: B.Handler a
  }

type NamedEventHandler a = (B.AddHandler a, NamedHandler a)

namedEventHandlerName :: NamedEventHandler a -> EventName
namedEventHandlerName neh = _namedHandlerName $ snd neh

instance Eq (NamedHandler a) where
  NamedHandler { _namedHandlerName = l } == NamedHandler { _namedHandlerName = r } = l == r

instance Ord (NamedHandler a) where
  compare NamedHandler { _namedHandlerName = l } NamedHandler { _namedHandlerName = r } = compare l r

instance Show (NamedHandler a) where
  show NamedHandler {..} = unpack _namedHandlerName

class Firable a b where
  fire :: a -> b -> IO ()

instance a ~ b => Firable (NamedHandler a) b where
  fire NamedHandler {..} = _namedHandlerHandler

instance a ~ b => Firable (B.Handler a) b where
  fire = id

newNamedEventHandler :: MonadIO m => Text -> m (NamedEventHandler a)
newNamedEventHandler name = liftIO $ do
  (ah, f) <- B.newAddHandler
  return (ah, NamedHandler name f)

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

data Script = Script
  { _scriptSuperScripts :: Vector ScriptName
  , _scriptName :: ScriptName
  , _scriptOnInit :: GameState -> B.MomentIO GameState
  , _scriptOnLoad :: GameState -> B.MomentIO GameState
  , _scriptOnEvent :: Vector (EventName, EndoName, GameState -> B.MomentIO GameState)
  , _scriptOnUnload :: GameState -> B.MomentIO GameState
  , _scriptOnExit :: GameState -> B.MomentIO GameState
  }

instance Eq Script where
  Script { _scriptName = sn1 } == Script { _scriptName = sn2 } = sn1 == sn2

instance Ord Script where
  compare Script { _scriptName = sn1 } Script { _scriptName = sn2 } = compare sn1 sn2

instance Show Script where
  show Script {..} = printf "<<Script \"%s\">>" (show _scriptName)

defaultScript :: Script
defaultScript = Script
  { _scriptSuperScripts = empty
  , _scriptName = error "Don't change this."
  , _scriptOnInit = return
  , _scriptOnLoad = return
  , _scriptOnEvent = empty
  , _scriptOnUnload = return
  , _scriptOnExit = return
  }

lookupEventByName :: EventName -> EventRegister -> Maybe (B.Event ())
lookupEventByName en (EventRegister er) = lookup en er

-- You can't really deregister events...
-- deregisterEventByName :: EventName -> EventRegister -> EventRegister
-- deregisterEventByName en (EventRegister er) = EventRegister $ MS.delete en er

registerEndoByName :: EventName -> EndoName -> (GameState -> B.MomentIO GameState) -> EventRegister -> EndoRegister -> EndoRegister
registerEndoByName eventName endoName endo eventR endoR =
  let me = lookupEventByName eventName eventR in
  case me of
    Nothing -> endoR
    Just e -> do
      let e' = const endo <$> e
      registerEndo endoName e' endoR

registerEndo :: EndoName -> B.Event (GameState -> B.MomentIO GameState) -> EndoRegister -> EndoRegister
registerEndo en e (EndoRegister er) = EndoRegister $ MS.insert en e er

-- TODO: Deregistering Endos should work now.

registerEvent :: NamedEventHandler () -> EventRegister -> B.MomentIO (EventRegister, NamedHandler ())
registerEvent neh (EventRegister er) = do
  let name = namedEventHandlerName neh
  (addHandler, fireHandle) <- liftIO $ newNamedEventHandler name
  e <- B.fromAddHandler addHandler
  let er' = EventRegister $ MS.insert name e er
  return (er', fireHandle)

data GraphicsContext = GraphicsContext
  { _graphicsContextClientAPI           :: G.ClientAPI
  , _graphicsContextContextVersionMajor :: Int
  , _graphicsContextContextVersionMinor :: Int
  , _graphicsContextContextRobustness   :: G.ContextRobustness
  , _graphicsContextOpenGLForwardCompat :: Bool
  , _graphicsContextOpenGLDebugContext  :: Bool
  , _graphicsContextOpenGLProfile       :: G.OpenGLProfile
  , _graphicsContextRefreshRate         :: Maybe Int
  , _graphicsContextRedBits             :: Int
  , _graphicsContextGreenBits           :: Int
  , _graphicsContextBlueBits            :: Int
  , _graphicsContextAlphaBits           :: Int
  , _graphicsContextDepthBits           :: Int
  , _graphicsContextStencilBits         :: Int
  , _graphicsContextSamples             :: Int
  , _graphicsContextStereo              :: Bool
  , _graphicsContextSRGBCapable         :: Bool
  } deriving (Eq, Ord, Show)

defaultGraphicsContext :: GraphicsContext
defaultGraphicsContext = GraphicsContext
  { _graphicsContextClientAPI           = G.ClientAPI'OpenGL
  , _graphicsContextContextVersionMajor = 4
  , _graphicsContextContextVersionMinor = 5
  , _graphicsContextContextRobustness   = G.ContextRobustness'NoRobustness
  , _graphicsContextOpenGLForwardCompat = True
  , _graphicsContextOpenGLDebugContext  = True
  , _graphicsContextOpenGLProfile       = G.OpenGLProfile'Core
  , _graphicsContextRefreshRate         = Nothing
  , _graphicsContextRedBits             = 8
  , _graphicsContextGreenBits           = 8
  , _graphicsContextBlueBits            = 8
  , _graphicsContextAlphaBits           = 8
  , _graphicsContextDepthBits           = 24
  , _graphicsContextStencilBits         = 8
  , _graphicsContextSamples             = 4
  , _graphicsContextStereo              = False
  , _graphicsContextSRGBCapable         = False
  }

data WindowConfig = WindowConfig
  { _windowConfigResizable          :: Bool
  , _windowConfigVisible            :: Bool
  , _windowConfigDecorated          :: Bool
  , _windowConfigWidth              :: Int
  , _windowConfigHeight             :: Int
  , _windowConfigTitle              :: String
  , _windowConfigMonitorFullscreen  :: Maybe G.Monitor
  , _windowConfigWindowContextShare :: Maybe G.Window
  } deriving (Eq, Ord, Show)

defaultWindowConfig :: WindowConfig
defaultWindowConfig = WindowConfig
  { _windowConfigResizable          = True
  , _windowConfigVisible            = True
  , _windowConfigDecorated          = True
  , _windowConfigWidth              = 1920
  , _windowConfigHeight             = 1080
  , _windowConfigTitle              = ""
  , _windowConfigMonitorFullscreen  = Nothing
  , _windowConfigWindowContextShare = Nothing
  }

data Player = Player
  { _playerPhysicsController :: P.KinematicCharacterController
  }

data PhysicsWorld = PhysicsWorld
  { _physicsWorldDynamicsWorld :: P.DynamicsWorld
  , _physicsWorldPlayers :: Vector Player
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

mconcat <$> mapM makeLenses
  [ ''Camera
  , ''NamedHandler
  , ''GameState
  , ''ExpandObjVTN
  , ''Script
  , ''ScriptName
  , ''EventRegister
  , ''EndoRegister
  , ''WindowConfig
  , ''GraphicsContext
  , ''Player
  , ''PhysicsWorld
  , ''VTNPoint, ''VTNIndex
  , ''MousePos
  ]
