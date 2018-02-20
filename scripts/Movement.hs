{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}

module Movement where

import           ClassyPrelude
import           Control.Lens
import           Foreign.C.Types
import           Game.Entity.Player
import           Game.Events
import           Game.Types
import           GHC.Float (double2Float)
import qualified Graphics.UI.GLFW as G
import qualified Linear as L

