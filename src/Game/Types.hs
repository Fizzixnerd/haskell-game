{-# LANGUAGE TemplateHaskell #-}

module Game.Types where

import Control.Lens
import qualified Linear as L

data Camera = Camera
  { _cameraPosition :: L.V3 Float
  , _cameraOrientation :: L.Quaternion Float
  , _cameraFOV :: Float
  }


mconcat <$> mapM makeLenses [''Camera]
