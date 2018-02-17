{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Main where

import           ClassyPrelude
import           Control.Concurrent
import           Game.Events
import           Game.Types
import           Game.Graphics.Model.Loader
import           Game.Graphics.Init
import           Game.Graphics.Rendering
import           Game.Graphics.Shader.Loader
import           Game.Graphics.Texture.Loader
import qualified Graphics.UI.GLFW             as G
import qualified Linear                       as L
import           Text.Printf
import           Game.Graphics.OpenGL.Binding

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

doItAndGimmeFireThing :: Game ( NamedHandler a
                              , NamedHandler G.Window
                              , G.Window
                              , IO () )
doItAndGimmeFireThing = do
  createGraphicsContext defaultGraphicsContext
  win <- createWindow defaultWindowConfig
  liftIO $ G.makeContextCurrent $ Just win

  cullFace $= Just Back
  depthFunc $= Just DepthLess
  let debugFunc src typ ident sever msg = printf "!!! OpenGL Error. Severity %s; ID: %s; Source: %s; Type: %s; Message: %s\n\n" (show sever) (show ident) (show src) (show typ) (show msg)
--- Remember: we will eventually have to free the function pointer that mkGLDEBUGPROC gives us!!!
  debugMessageCallback $= Just debugFunc
  printContextVersion win

  prog <- compileShaders

  (objPoints, objIndices) <- loadObjVTN "res/models/simple-cube-2.obj"

  tex <- loadBMPTexture "res/models/simple-cube-2.bmp"

  let posLocation = AttribLocation 0
      texLocation = AttribLocation 1
      nmlLocation = AttribLocation 2
  (vao, _, ebuf) <- bufferData posLocation texLocation nmlLocation objPoints objIndices

  clearColor $= color4 0 0 0.4 0
--  GL.viewport $= (GL.Position 0 0, GL.Size 1920 1080)

  let texSampleLoc = TextureUnit 0
  (hello, shouldClose, key, mouseData, tick) <- compileGameNetwork prog texSampleLoc vao ebuf tex

  liftIO $ G.setWindowCloseCallback win (Just $ fire shouldClose)
  liftIO $ G.setKeyCallback win (Just (\w k sc ks mk -> fire key (w, k, sc, ks, mk)))
  let mouseCallBack _ x y = fire mouseData $ MousePos (L.V2 x y)
  liftIO $ G.setCursorPosCallback win $ Just mouseCallBack
  liftIO $ G.setCursorInputMode win G.CursorInputMode'Disabled
  let someFunc' :: IO ()
      someFunc' = do
        loop win
        deleteObjectName vao
        deleteObjectName prog
        G.terminate
          where
            loop w = do
              fire tick w
              G.pollEvents
              sc <- G.windowShouldClose w
              unless sc $ loop w

  return (hello, shouldClose, win, someFunc')
{-
game :: Game ( NamedHandler ()
             , IO ()
             , ThreadId )
-}
game :: Game (IO ())
game = do
  (h, sc, w, x) <- doItAndGimmeFireThing
  return x
--  ti <- liftIO $ forkIO x
--  return (h, fire sc w, ti)

someFunc :: IO ()
someFunc = join (runGame game)
