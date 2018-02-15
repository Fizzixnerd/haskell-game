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
import           Game.Entity.Player
import           Game.World.Physics
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
  -> m (NamedHandler b1, NamedHandler G.Window,
        NamedHandler (G.Window, G.Key, ScanCode, G.KeyState, G.ModifierKeys),
        NamedHandler (Double, Double), NamedHandler G.Window)
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

        p <- newPlayer
        pw <- newPhysicsWorld
        pw' <- addPlayerToPhysicsWorld p pw
        let eStepPhysicsWorld :: B.Event (IO ())
            eStepPhysicsWorld = const (void $ stepPhysicsWorld pw') <$> eTick

        gameState <- let installInputEvents gs = gs & gameStateMousePosEvent .~ eMousePos
                                                    & gameStateKeyEvent .~ eKey
                                                    & gameStatePlayer .~ p
                                                    & gameStatePhysicsWorld .~ pw'
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

        B.reactimate ePrintHello
        B.reactimate eClose
        B.reactimate eRender
        B.reactimate eEscapeToClose
        B.reactimate eStepPhysicsWorld

  net <- liftIO $ B.compile network
  liftIO $ B.actuate net
  return (hello, shouldClose, key, mousePos, tick)

installKeyEventListener :: ((G.Window, G.Key, ScanCode, G.KeyState, G.ModifierKeys) -> GameState -> B.MomentIO GameState)
                        -> EndoName
                        -> GameState
                        -> B.MomentIO GameState
installKeyEventListener el en gs =
  return $ gs & gameStateEndoRegister %~ registerEndo en (el <$> (gs ^. gameStateKeyEvent))

installMouseEventListener :: ((Double, Double) -> GameState -> B.MomentIO GameState) -> EndoName -> GameState -> B.MomentIO GameState
installMouseEventListener el en gs =
  return $ gs & gameStateEndoRegister %~ registerEndo en (el <$> (gs ^. gameStateMousePosEvent))
