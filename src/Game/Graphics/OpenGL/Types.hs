{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Game.Graphics.OpenGL.Types where

import Graphics.GL.Types
import Graphics.GL.Core45

import ClassyPrelude
import Control.Lens
import Data.Bits
import Foreign.Ptr
import Game.Graphics.OpenGL.Utils
import qualified Data.Vector.Storable as VS
