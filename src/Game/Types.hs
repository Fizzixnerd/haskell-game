{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Game.Types where

import ClassyPrelude
import Control.Lens

import qualified Reactive.Banana.Frameworks as B
import qualified Linear as L

data Camera = Camera
  { _cameraPosition :: L.V3 Float
  , _cameraOrientation :: L.Quaternion Float
  , _cameraFOV :: Float
  }

data NamedHandler a = NamedHandler
  { _namedHandlerName :: Text
  , _namedHandlerHandler :: B.Handler a
  }

type NamedEventHandler = (B.AddHandler, NamedHandler)

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

newNamedEventHandler :: Text -> IO (NamedEventHandler a)
newNamedEventHandler name = do
  (ah, f) <- B.newAddHandler
  return (ah, NamedHandler name f)

mconcat <$> mapM makeLenses [''Camera, ''NamedHandler]
