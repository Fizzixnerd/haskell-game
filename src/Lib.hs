{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import ClassyPrelude
import Graphics.UI.GLFW as GLFW
import Data.Maybe

someFunc :: IO ()
someFunc = do
  GLFW.init
  mwin <- createWindow 640 640 "Haskell Game Hello World" Nothing Nothing
  case mwin of
    Nothing -> error "Could not create window."
    Just win -> do
      makeContextCurrent mwin
      setWindowCloseCallback win (Just $ \w -> setWindowShouldClose w True)
      listenForEvents win
  where
    listenForEvents w = do
      pollEvents
      sc <- windowShouldClose w
      when sc $
        terminate
      listenForEvents w
