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

import ClassyPrelude
import Control.Lens

import Data.Data
import qualified Control.Monad.Logger as ML
import qualified Reactive.Banana.Frameworks as B
import qualified Linear as L

newtype Game a = Game { _unGame :: ML.LoggingT IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , ML.MonadLogger
           , MonadIO )

runGame :: Game a -> IO a
runGame g = ML.runStderrLoggingT $ _unGame g

data GameState = GameState
  { _gameStateCamera :: Camera
  } deriving (Eq, Ord, Show)

data Camera = Camera
  { _cameraPosition :: L.V3 Float
  , _cameraOrientation :: (Float, Float)
  , _cameraFOV :: Float
  } deriving (Eq, Show, Ord)

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
  { _namedHandlerName :: Text
  , _namedHandlerHandler :: B.Handler a
  }

type NamedEventHandler a = (B.AddHandler a, NamedHandler a)

instance Eq (NamedHandler a) where
  (NamedHandler { _namedHandlerName = l }) == (NamedHandler { _namedHandlerName = r }) = l == r

instance Ord (NamedHandler a) where
  compare (NamedHandler { _namedHandlerName = l }) (NamedHandler { _namedHandlerName = r }) = compare l r

instance Show (NamedHandler a) where
  show (NamedHandler {..}) = unpack _namedHandlerName

class Firable a b where
  fire :: a -> b -> IO ()

instance a ~ b => Firable (NamedHandler a) b where
  fire (NamedHandler {..}) b = _namedHandlerHandler b

instance a ~ b => Firable (B.Handler a) b where
  fire f b = f b

newNamedEventHandler :: MonadIO m => Text -> m (NamedEventHandler a)
newNamedEventHandler name = liftIO $ do
  (ah, f) <- B.newAddHandler
  return (ah, NamedHandler name f)

mconcat <$> mapM makeLenses [ ''Camera
                            , ''NamedHandler
                            , ''Game
                            , ''GameState ]

-- This is here for a plugin test
data X = X
  { _x :: Int
  } deriving (Eq, Ord, Show, Data, Typeable)

