{-# LANGUAGE NoImplicitPrelude #-}

module ForeignScript where

import ClassyPrelude
import Game.Types (Script(..))

script :: Script
script = Script
  { _scriptSuperScripts = empty
  , _scriptName = undefined
  , _scriptOnInit = id
  , _scriptOnLoad = id
  , _scriptOnEvent = empty
  , _scriptOnUnload = id
  , _scriptOnExit = id
  }
