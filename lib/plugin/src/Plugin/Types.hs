{-# LANGUAGE TemplateHaskell #-}

module Plugin.Types where

import Control.Lens

type ModName = String
type ValName = String

data Plugin = Plugin 
  { _pluginDirectory :: FilePath
  , _pluginModule :: ModName
  , _pluginValue  :: ValName
  } deriving (Eq, Ord, Show)

makeLenses ''Plugin
