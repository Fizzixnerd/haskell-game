{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Events where

import           ClassyPrelude
import           Control.Lens
import           Game.Graphics.Rendering
import           Game.Types
import           Game.Script.Loader
import           Game.Script.Installer
import           Game.Graphics.Binding
import           Game.Entity.Player
import           Game.World.Physics
import qualified Graphics.UI.GLFW             as G
import qualified Reactive.Banana.Combinators  as B
import qualified Reactive.Banana.Frameworks   as B
import qualified Linear                       as L
import GHC.Float (double2Float)

mousePosToRot :: Float -> MousePos -> MousePos -> Double -> (Float, Float)
mousePosToRot mouseSpeed (MousePos (L.V2 newx newy)) (MousePos (L.V2 oldx oldy)) dt = if abs xrot > 0.1 || abs yrot > 0.1
  then trace (show (xrot, yrot, dt) <> "\n") (xrot, yrot)
  else (xrot,yrot)
  where
    notTooSmall x = if abs x < 0.01 then 0 else x
    clampy a b = max a . min b
    xrot = mouseSpeed * double2Float (dt * notTooSmall (oldx - newx))
    yrot = mouseSpeed * double2Float (dt * notTooSmall (oldy - newy))

rotateCamera :: (Float, Float) -> Camera -> Camera
rotateCamera (dhor, dver) cam = cam & cameraOrientation %~ go
  where
    go (hor, ver) = (hor + dhor, max (-pi/2) . min (pi/2) $ ver + dver)


compileGameNetwork ::
  MonadIO m =>
  Program
  -> TextureUnit
  -> VertexArrayObject
  -> (BufferObject, Int)
  -> TextureObject TextureTarget2D
  -> m (NamedHandler b1, NamedHandler G.Window,
        NamedHandler (G.Window, G.Key, ScanCode, G.KeyState, G.ModifierKeys), NamedHandler MousePos, NamedHandler G.Window)
compileGameNetwork prog texSampleLoc vao ebuf tex = do
  -- get the Handlers we need.
  (addHandlerShouldClose, shouldClose) <- newNamedEventHandler "shouldClose"
  (addHandlerTick, tick) <- newNamedEventHandler "tick"
  (addHandlerKey, key) <- newNamedEventHandler "key"
  (addHandlerHello, hello) <- newNamedEventHandler "hello"
  (addHandlerMouseData, mouseData) <- newNamedEventHandler "mouseData"

  let network :: B.MomentIO ()
      network = mdo
        eTick <- B.fromAddHandler addHandlerTick
        eShouldClose <- B.fromAddHandler addHandlerShouldClose
        eKey <- B.fromAddHandler addHandlerKey
        eHello <- B.fromAddHandler addHandlerHello
        eMouseData <- B.fromAddHandler addHandlerMouseData

        let eClose :: B.Event (IO ())
            eClose = flip G.setWindowShouldClose True <$> eShouldClose

            renderer :: MonadIO m => GameState -> G.Window -> m ()
            renderer gs w = liftIO $ do
              render gs prog texSampleLoc vao (snd ebuf) tex
              G.swapBuffers w

            eRender = B.apply ((\gs w -> do
                                   render gs prog texSampleLoc vao (snd ebuf) tex
                                   G.swapBuffers w) <$> bWorld) eTick

            eEscapeToClose :: B.Event (IO ())
            eEscapeToClose = (\(w, _b, _, _, _) -> G.setWindowShouldClose w True)
              <$> B.filterE (\(_, k, _, _, _) -> k == G.Key'Escape) eKey

            ePrintHello :: B.Event (IO ())
            ePrintHello = const (print ("hello" :: String)) <$> eHello

            camHandler :: MousePos -> GameState -> B.MomentIO GameState
            camHandler mouseP gs = return $ gs & gameStateCamera %~ (rotateCamera . mousePosToRot (gs^.gameStateMouseSpeed) mouseP (gs^.gameStateMousePos) $ (1/60)) & gameStateMousePos .~ mouseP
            eCamOrientation :: B.Event (GameState -> B.MomentIO GameState)
            eCamOrientation = camHandler <$> eMouseData
        eWorld <- B.accumE (pure initGameState) (fmap (=<<) eCamOrientation)
        eWorld' <- B.execute eWorld
        bWorld <- B.stepper initGameState eWorld'
        B.reactimate ePrintHello
        B.reactimate eClose
        B.reactimate eRender
        B.reactimate eEscapeToClose

  net <- liftIO $ B.compile network
  liftIO $ B.actuate net
  return (hello, shouldClose, key, mouseData, tick)

installKeyEventListener :: ((G.Window, G.Key, ScanCode, G.KeyState, G.ModifierKeys) -> GameState -> B.MomentIO GameState)
                        -> EndoName
                        -> GameState
                        -> B.MomentIO GameState
installKeyEventListener el en gs =
  return $ gs & gameStateEndoRegister %~ registerEndo en (el <$> (gs ^. gameStateKeyEvent))

{-
installMouseEventListener :: ((MousePos, Double) -> GameState -> B.MomentIO GameState) -> EndoName -> GameState -> B.MomentIO GameState
installMouseEventListener el en gs =
  return $ gs & gameStateEndoRegister %~ registerEndo en (el <$> (gs ^. gameStateMousePosEvent))
-}
