{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Main where

import           ClassyPrelude
import           Control.Concurrent
import           Data.Maybe
import           Game.Events
import           Game.Types
import           Game.Graphics.Model.Loader
import           Game.Graphics.Texture.Loader
import           Game.Graphics.Shader.Loader
import           Game.Graphics.Rendering
import qualified Linear.OpenGL                as L ()
import qualified Graphics.UI.GLFW             as G
import qualified Graphics.Rendering.OpenGL.GL as G
import           Text.Printf

graphicsInit :: MonadIO m => m ()
graphicsInit = liftIO $ do
  _ <- G.init
  G.windowHint $ G.WindowHint'ContextVersionMajor 4
  G.windowHint $ G.WindowHint'ContextVersionMinor 5
  G.windowHint $ G.WindowHint'OpenGLForwardCompat True
  G.windowHint $ G.WindowHint'Samples 4
  G.windowHint $ G.WindowHint'OpenGLProfile G.OpenGLProfile'Core
  G.windowHint $ G.WindowHint'OpenGLDebugContext True
  G.windowHint $ G.WindowHint'DepthBits 24

withWindow :: MonadIO m => (G.Window -> m a) -> m a
withWindow f = do
  mwin <- liftIO $ G.createWindow 1920 1080 "Haskell Game Hello World" Nothing Nothing
  case mwin of
    Nothing -> error "Could not create window."
    Just win -> do
      x <- f win
      liftIO $ G.destroyWindow win
      return x

printContextVersion :: MonadIO m => G.Window -> m ()
printContextVersion win = liftIO $ do
  maj <- G.getWindowContextVersionMajor win
  min_ <- G.getWindowContextVersionMinor win
  rev <- G.getWindowContextVersionRevision win
  printf "%i.%i.%i\n" maj min_ rev

doItAndGimmeFireThing :: Game ( NamedHandler ()
                              , NamedHandler ()
                              , NamedHandler ()
                              , NamedHandler G.Window
                              , G.Window
                              , IO () )
doItAndGimmeFireThing = do
  graphicsInit
  mWin <- liftIO $ G.createWindow 1920 1080 "Haskell Game Hello World" Nothing Nothing
  let win = fromJust mWin
  liftIO $ G.makeContextCurrent $ Just win

  liftIO $ G.setCursorInputMode win G.CursorInputMode'Disabled
  G.cullFace G.$= Just G.Back
  G.depthFunc G.$= Just G.Less
  G.debugMessageCallback G.$= Just (printf "!!!%s!!!\n\n" . show)
  printContextVersion win

  prog <- compileShaders
  -- This is the bad way of doing this.
  mvpLoc <- liftIO $ G.uniformLocation prog "MVP"
  texSampleLoc <- liftIO $ G.uniformLocation prog "texSampler"
  (objPoints, objIndices) <- loadObjVTN "res/models/simple-cube-2.obj"

  tex <- loadBMPTexture "res/models/simple-cube-2.bmp"

  let posLocation = G.AttribLocation 0
      texLocation = G.AttribLocation 1
      nmlLocation = G.AttribLocation 2

  (vao, _, ebuf) <- bufferData posLocation texLocation nmlLocation objPoints objIndices

  G.clearColor G.$= G.Color4 0 0 0.4 0
  G.viewport G.$= (G.Position 0 0, G.Size 1920 1080)

  (hello, gameReset, shouldClose, key, mousePos, tick, fuckWithFoV) <- compileGameNetwork prog mvpLoc texSampleLoc vao ebuf tex

  liftIO $ G.setWindowCloseCallback win (Just $ fire shouldClose)
  liftIO $ G.setKeyCallback win (Just (\w k sc ks mk -> fire key (w, k, sc, ks, mk)))
  liftIO $ G.setCursorPosCallback win (Just (\w x y -> fire mousePos (w, x, y) >> G.setCursorPos w (1920 / 2) (1080 / 2)))

  let someFunc' :: IO ()
      someFunc' = do
        loop win
        G.deleteObjectName vao
        G.deleteObjectName prog
        G.terminate
          where
            loop w = do
              fire tick w
              G.pollEvents
              sc <- G.windowShouldClose w
              unless sc $ loop w

  return (hello, gameReset, fuckWithFoV, shouldClose, win, someFunc')

game :: Game ( NamedHandler ()
             , NamedHandler ()
             , NamedHandler ()
             , IO ()
             , ThreadId)
game = do
  (h, r, fwf, sc, w, x) <- doItAndGimmeFireThing
  ti <- liftIO $ forkIO x
  return (h, r, fwf, fire sc w, ti)

someFunc :: IO ()
someFunc = void $ runGame game
