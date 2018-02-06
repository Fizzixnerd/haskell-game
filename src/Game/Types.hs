{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Types where

import ClassyPrelude
import Control.Lens

import qualified Control.Monad.Logger as ML
import qualified Control.Monad.State.Strict as MS
import qualified Reactive.Banana.Frameworks as B
import qualified Linear as L

newtype Game s a = Game { unGame :: ML.LoggingT (MS.StateT s IO) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MS.MonadState s
           , ML.MonadLogger
           , MonadIO )

runGame :: s -> Game s a -> IO (a, s)
runGame s g = MS.runStateT (ML.runStderrLoggingT $ unGame g) s

data Camera = Camera
  { _cameraPosition :: L.V3 Float
  , _cameraOrientation :: L.Quaternion Float
  , _cameraFOV :: Float
  } deriving (Eq, Show, Ord)

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

mconcat <$> mapM makeLenses [''Camera, ''NamedHandler]
