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
import           Game.Graphics.OpenGL.Binding
import qualified Graphics.UI.GLFW             as G
import qualified Reactive.Banana.Combinators  as B
import qualified Reactive.Banana.Frameworks   as B

plainChanges :: B.Behavior a -> B.MomentIO (B.Event a)
plainChanges b = do
  (e, handle_) <- B.newEvent
  eb <- B.changes b
  B.reactimate' $ (fmap handle_) <$> eb
  return e

compileGameNetwork ::
  MonadIO m =>
  Program
  -> TextureUnit
  -> VertexArrayObject
  -> (BufferObject, Int)
  -> TextureObject TextureTarget2D
  -> m (NamedHandler b1, NamedHandler G.Window,
        NamedHandler (G.Window, G.Key, ScanCode, G.KeyState, G.ModifierKeys),
        NamedHandler (G.Window, Double, Double), NamedHandler G.Window)
compileGameNetwork prog texSampleLoc vao ebuf tex = do
  -- get the Handlers we need.
  (addHandlerShouldClose, shouldClose) <- newNamedEventHandler "shouldClose"
  (addHandlerTick, tick) <- newNamedEventHandler "tick"
  (addHandlerKey, key) <- newNamedEventHandler "key"
  (addHandlerHello, hello) <- newNamedEventHandler "hello"
  (addHandlerMousePos, mousePos) <- newNamedEventHandler "mousePos"

  movementScript <- liftIO $ loadForeignScript $ ScriptName "scripts" "Movement"
  let installMovementScript = scriptInstall movementScript

  let network :: B.MomentIO ()
      network = mdo
        eTick <- B.fromAddHandler addHandlerTick
        eShouldClose <- B.fromAddHandler addHandlerShouldClose
        eKey <- B.fromAddHandler addHandlerKey
        eHello <- B.fromAddHandler addHandlerHello
        eMousePos <- B.fromAddHandler addHandlerMousePos

        let eClose :: B.Event (IO ())
            eClose = flip G.setWindowShouldClose True <$> eShouldClose

            eRender :: B.Event (IO ())
            eRender = B.apply ((\gs w -> do
                                   render gs prog texSampleLoc vao (snd ebuf) tex
                                   G.swapBuffers w) <$> bWorld) eTick

            eEscapeToClose :: B.Event (IO ())
            eEscapeToClose = (\(w, _b, _, _, _) -> G.setWindowShouldClose w True)
              <$> B.filterE (\(_, k, _, _, _) -> k == G.Key'Escape) eKey

            ePrintHello :: B.Event (IO ())
            ePrintHello = const (print ("hello" :: String)) <$> eHello

        gameState <- let installInputEvents gs = gs & gameStateMousePosEvent .~ eMousePos
                                                    & gameStateKeyEvent .~ eKey
                     in
                       installMovementScript $ installInputEvents initGameState  

        --  b w -> b (w -> mio w), e w -> e (mio w) -> mio (e w) -+
        --  ^                                                     |
        --  |                                                     |
        --  +-----------------------------------------------------+
        bWorld <- B.stepper gameState eNewWorld
        let eWorld = (const <$> bWorld) B.<@> eTick
        let eUpdater = (\w -> B.unions $
                               fmap (fmap (>=>)) $
                               toList $
                               w ^. gameStateEndoRegister . unEndoRegister) <$> eWorld
        eUpdater' <- B.switchE eUpdater
        bUpdater <- B.accumB return eUpdater'
        let eUpdateWorld = bUpdater B.<@> eWorld
        eNewWorld <- B.execute eUpdateWorld

        B.reactimate ((print . _gameStateBackgroundColor) <$> eWorld)
        B.reactimate ePrintHello
        B.reactimate eClose
        B.reactimate eRender
        B.reactimate eEscapeToClose

  net <- liftIO $ B.compile network
  liftIO $ B.actuate net
  return (hello, shouldClose, key, mousePos, tick)
