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

compileGameNetwork ::
  MonadIO m =>
  Program
  -> TextureUnit
  -> VertexArrayObject
  -> (BufferObject, Int)
  -> TextureObject TextureTarget2D
  -> m (NamedHandler b1, NamedHandler b2, NamedHandler G.Window,
        NamedHandler (G.Window, G.Key, ScanCode, G.KeyState, G.ModifierKeys),
        NamedHandler (G.Window, Double, Double), NamedHandler G.Window)
compileGameNetwork prog texSampleLoc vao ebuf tex = do
  -- get the Handlers we need.
  (addHandlerShouldClose, shouldClose) <- newNamedEventHandler "shouldClose"
  (addHandlerTick, tick) <- newNamedEventHandler "tick"
  (addHandlerKey, key) <- newNamedEventHandler "key"
  (addHandlerHello, hello) <- newNamedEventHandler "hello"
  (addHandlerMousePos, mousePos) <- newNamedEventHandler "mousePos"
  (addHandlerGameReset, gameReset) <- newNamedEventHandler "gameReset"

  movementScript <- liftIO $ loadForeignScript $ ScriptName "scripts" "Movement"
  let installMovementScript = scriptInstall movementScript

  let network :: B.MomentIO ()
      network = mdo
        eTick <- B.fromAddHandler addHandlerTick
        eShouldClose <- B.fromAddHandler addHandlerShouldClose
        eKey <- B.fromAddHandler addHandlerKey
        eHello <- B.fromAddHandler addHandlerHello
        eMousePos <- B.fromAddHandler addHandlerMousePos
        eGameReset <- B.fromAddHandler addHandlerGameReset

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

            gameState :: GameState
            gameState = installMovementScript $
                        installInputEvents $ initGameState
              where installInputEvents gs = gs & gameStateMousePosEvent .~ eMousePos
                                               & gameStateKeyEvent .~ eKey

            eResetGame :: B.Event (GameState -> GameState)
            eResetGame = const (const gameState) <$> eGameReset

        bWorld <- B.accumB gameState (B.unions $ toList $ gameState ^. gameStateEndoRegister . unEndoRegister)

        B.reactimate ePrintHello
        B.reactimate eClose
        B.reactimate eRender
        B.reactimate eEscapeToClose

  net <- liftIO $ B.compile network
  liftIO $ B.actuate net
  return (hello, gameReset, shouldClose, key, mousePos, tick)
