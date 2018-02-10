{-# LANGUAGE NoImplicitPrelude #-}

module ForeignScript where

import ClassyPrelude
import Game.Types

script :: Script
script = Script
  { _scriptSuperScripts = empty
  , _scriptName = undefined
  , _scriptOnInit = return
  , _scriptOnLoad = return
  , _scriptOnEvent = empty
  , _scriptOnUnload = return
  , _scriptOnExit = return
  }
