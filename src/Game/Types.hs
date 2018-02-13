{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Game.Types
  ( module Game.Types
  , module Game.StorableTypes
  ) where

import           ClassyPrelude
import qualified Codec.Wavefront             as W
import           Control.Lens
import qualified Control.Monad.Logger        as ML
import qualified Data.Map.Strict             as MS
import           Foreign.C.Types
import           Game.StorableTypes
import qualified Graphics.UI.GLFW            as G
import qualified Linear                      as L
import qualified Reactive.Banana.Combinators as B
import qualified Reactive.Banana.Frameworks  as B
import           Text.Printf

newtype Game a = Game { _unGame :: ML.LoggingT IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , ML.MonadLogger
           , MonadIO )

runGame :: Game a -> IO a
runGame g = ML.runStderrLoggingT $ _unGame g

newtype EventRegister = EventRegister { _unEventRegister :: MS.Map EventName (B.Event ()) }
newtype EndoRegister  = EndoRegister { _unEndoRegister :: MS.Map EndoName (B.Event (GameState -> GameState)) }

type ScanCode = Int

data GameState = GameState
  { _gameStateCamera :: Camera
  , _gameStateActiveScripts :: Vector Script
  , _gameStateEventRegister :: EventRegister
  , _gameStateEndoRegister  :: EndoRegister
  , _gameStateMousePosEvent :: B.Event (G.Window, Double, Double)
  , _gameStateKeyEvent      :: B.Event (G.Window, G.Key, ScanCode, G.KeyState, G.ModifierKeys)
  }

initGameState :: GameState
initGameState = GameState
  { _gameStateCamera = Camera
    { _cameraPosition = L.V3 0 0 2
    , _cameraOrientation = (0, 0)
    , _cameraFOV = pi/2 }
  , _gameStateActiveScripts = empty
  , _gameStateEventRegister = EventRegister mempty
  , _gameStateEndoRegister  = EndoRegister mempty
  , _gameStateMousePosEvent = error "mousePosEvent not set."
  , _gameStateKeyEvent      = error "keyEvent not set."
  }

data Camera = Camera
  { _cameraPosition :: L.V3 Float
  , _cameraOrientation :: (Float, Float)
  , _cameraFOV :: Float
  } deriving (Eq, Show, Ord)

cameraMVP :: Getter Camera (L.M44 Float)
cameraMVP = to go
  where
    go (Camera vpos (vangh, vangv) cfov) = camPerspective L.!*! camView L.!*! camModel
      where
        vup  = L.V3 0 1 0
        vdir = L.rotate (L.axisAngle (L.V3 0 1 0) vangh * L.axisAngle (L.V3 1 0 0) vangv) (L.V3 0 0 (negate 1))
        camModel = L.identity
        camView = L.lookAt vpos (vpos + vdir) vup
      -- Projection matrix : 90deg Field of View, 16:9 ratio, display range : 0.1 unit <-> 100 units
        camPerspective = L.perspective cfov (16/9) 0.1 100

data Movement =
    MoveLeft
  | MoveRight
  | MoveForward
  | MoveBackward
  | MoveUp
  | MoveDown
  | MoveCameraDir Double Double
  deriving (Eq, Show, Ord)

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
  , _scriptOnInit :: GameState -> GameState
  , _scriptOnLoad :: GameState -> GameState
  , _scriptOnEvent :: Vector (EventName, EndoName, GameState -> GameState)
  , _scriptOnUnload :: GameState -> GameState
  , _scriptOnExit :: GameState -> GameState
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
  , _scriptOnInit = id
  , _scriptOnLoad = id
  , _scriptOnEvent = empty
  , _scriptOnUnload = id
  , _scriptOnExit = id
  }

lookupEventByName :: EventName -> EventRegister -> Maybe (B.Event ())
lookupEventByName en (EventRegister er) = lookup en er

-- You can't really deregister events...
-- deregisterEventByName :: EventName -> EventRegister -> EventRegister
-- deregisterEventByName en (EventRegister er) = EventRegister $ MS.delete en er

registerEndoByName :: EventName -> EndoName -> (GameState -> GameState) -> EventRegister -> EndoRegister -> EndoRegister
registerEndoByName eventName endoName endo eventR endoR =
  let me = lookupEventByName eventName eventR in
  case me of
    Nothing -> endoR
    Just e -> do
      let e' = const endo <$> e
      registerEndo endoName e' endoR

registerEndo :: EndoName -> B.Event (GameState -> GameState) -> EndoRegister -> EndoRegister
registerEndo en e (EndoRegister er) = EndoRegister $ MS.insert en e er

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
  }

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
  }

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
  ]
