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
<<<<<<< HEAD
import qualified Reactive.Banana.Frameworks as B

camHandler :: MousePos -> GameState -> B.MomentIO GameState
camHandler mouseP gs = return $ gs & gameStateCamera %~ (rotateCamera . mousePosToRot (gs^.gameStateMouseSpeed) mouseP (gs^.gameStateMousePos) $ (1/60)) 
=======
import qualified Linear as L
import qualified Reactive.Banana.Frameworks as B

camHandler :: MousePos -> GameState -> B.MomentIO GameState
camHandler mouseP gs = return $ gs & gameStateCamera %~ (rotateCamera . mousePosToRot (gs^.gameStateMouseSpeed) mouseP (gs^.gameStateMousePos) $ (1/60))
>>>>>>> camera_work
                                   & gameStateMousePos .~ mouseP

script :: Script
script = defaultScript
<<<<<<< HEAD
  { _scriptOnInit = installMouseEventListener camHandler "mouseMovement" }
=======
  { _scriptOnInit = installMouseEventListener camHandler "mouseMovement"
  }
>>>>>>> camera_work
