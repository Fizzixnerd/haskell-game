{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import ClassyPrelude
import qualified Graphics.UI.GLFW as G
import Data.Maybe

someFunc :: IO ()
someFunc = do
  G.init
  mwin <- G.createWindow 1920 1080 "Haskell Game Hello World" Nothing Nothing
  case mwin of
    Nothing -> error "Could not create window."
    Just win -> do
      G.makeContextCurrent mwin
      G.setWindowCloseCallback win (Just $ \w -> G.setWindowShouldClose w True)
      listenForEvents win
  where
    listenForEvents w = do
      G.pollEvents
      sc <- G.windowShouldClose w
      when sc $
        G.terminate
      listenForEvents w
