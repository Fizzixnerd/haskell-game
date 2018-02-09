{-# LANGUAGE OverloadedStrings #-}

module ForeignEvent where

import Linear
import Game.Types

gs :: GameState -> GameState
gs = let gameState = GameState
           { _gameStateCamera = Camera
             { _cameraPosition = V3 0 0 2
             , _cameraOrientation = (0, 0)
             , _cameraFOV = pi/3 } }
     in
       const gameState

