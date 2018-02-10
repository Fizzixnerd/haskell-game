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

newtype EventRegister = EventRegister { _unEventRegister :: MS.Map EventName RegisteredEvent }
newtype RegisteredEvent = RegisteredEvent { _unRegisteredEvent :: B.Event () }

lookupEventByName :: EventName -> EventRegister -> Maybe (B.Event ())
lookupEventByName en (EventRegister er) = _unRegisteredEvent <$> (lookup en er)

deregisterEventByName :: EventName -> EventRegister -> EventRegister
deregisterEventByName en (EventRegister er) = EventRegister $ MS.delete en er

registerEvent :: NamedEventHandler () -> EventRegister -> B.MomentIO (EventRegister, NamedHandler ())
registerEvent neh (EventRegister er) = do
  let name = namedEventHandlerName neh
  (addHandler, fireHandle) <- liftIO $ newNamedEventHandler name
  e <- B.fromAddHandler addHandler
  let er' = EventRegister $ MS.insert name (RegisteredEvent e) er
  return (er', fireHandle)
  

data GameState = GameState
  { _gameStateCamera :: Camera
  , _gameStateActiveScripts :: Vector Script
  , _gameStateEventRegister :: EventRegister
  }

initGameState :: GameState
initGameState = GameState
  { _gameStateCamera = Camera
    { _cameraPosition = L.V3 0 0 2
    , _cameraOrientation = (0, 0)
    , _cameraFOV = pi/2 }
  , _gameStateActiveScripts = empty
  , _gameStateEventRegister = EventRegister mempty
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

data ScriptName = ScriptName
  { _scriptNamePath :: FilePath
  , _scriptNameMainModule :: ModuleName
  } deriving (Eq, Ord, Show)

data Script = Script
  { _scriptSuperScripts :: Vector ScriptName
  , _scriptName :: ScriptName
  , _scriptOnInit :: GameState -> Game GameState
  , _scriptOnLoad :: GameState -> Game GameState
  , _scriptOnEvent :: Vector (EventName, GameState -> Game GameState)
  , _scriptOnUnload :: GameState -> Game GameState
  , _scriptOnExit :: GameState -> Game GameState
  }

instance Eq Script where
  (Script { _scriptName = sn1 }) == (Script { _scriptName = sn2 }) = sn1 == sn2

instance Ord Script where
  compare (Script { _scriptName = sn1 }) (Script { _scriptName = sn2 }) = compare sn1 sn2

instance Show Script where
  show (Script {..}) = printf "<<Script \"%s\">>" (show _scriptName)

mconcat <$> mapM makeLenses [''Camera, ''NamedHandler, ''GameState, ''ExpandObjVTN, ''Script, ''ScriptName, ''EventRegister, ''RegisteredEvent]
