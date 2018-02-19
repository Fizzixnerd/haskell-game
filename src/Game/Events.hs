{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}

module Game.Events where

import           Control.Arrow
import qualified Control.Wire as N
import qualified FRP.Netwire as N
import qualified FRP.Netwire.Input as N
import qualified FRP.Netwire.Input.GLFW as N
import qualified Graphics.UI.GLFW as G
import           ClassyPrelude
import           Control.Lens
import           Game.Graphics.Rendering
import           Game.Types
import           Game.Script.Loader
import           Game.Script.Installer
import           Game.Graphics.Binding
import           Game.Entity.Player
import           Game.World.Physics
import qualified Linear                       as L
import           GHC.Float (double2Float)

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

camera :: GameWire s a Camera
camera = rotCam <<< N.mouseMickies
  where
    rotCam = N.mkGen_ (\micky -> Right <$> (gameStateCamera <%= rotateCamera micky))

mouse'L :: GameWire s a a
mouse'L = N.mousePressed G.MouseButton'1

mouse'R :: GameWire s a a
mouse'R = N.mousePressed G.MouseButton'2

key'w :: GameWire s a a
key'w = N.keyPressed G.Key'W

key'a :: GameWire s a a
key'a = N.keyPressed G.Key'A

key's :: GameWire s a a
key's = N.keyPressed G.Key'S

key'd :: GameWire s a a
key'd = N.keyPressed G.Key'D

printShit :: GameWire s a ()
printShit = N.mkGen_ printIt
  where
    printIt _ = Right <$> liftIO (print ("fuck" :: String))

printOut :: GameWire s a ()
printOut = printShit <<< key'w

-- compileGameNetwork ::
--   MonadIO m =>
--   Program
--   -> TextureUnit
--   -> VertexArrayObject
--   -> (BufferObject, Int)
--   -> TextureObject TextureTarget2D
--   -> m (NamedHandler b1, NamedHandler G.Window,
--         NamedHandler (G.Window, G.Key, ScanCode, G.KeyState, G.ModifierKeys), NamedHandler MousePos, NamedHandler G.Window)
-- compileGameNetwork prog texSampleLoc vao ebuf tex = do
--   -- get the Handlers we need.
--   (addHandlerShouldClose, shouldClose) <- newNamedEventHandler "shouldClose"
--   (addHandlerTick, tick) <- newNamedEventHandler "tick"
--   (addHandlerKey, key) <- newNamedEventHandler "key"
--   (addHandlerHello, hello) <- newNamedEventHandler "hello"
--   (addHandlerMouseData, mouseData) <- newNamedEventHandler "mouseData"

--   let network :: B.MomentIO ()
--       network = mdo
--         eTick <- B.fromAddHandler addHandlerTick
--         eShouldClose <- B.fromAddHandler addHandlerShouldClose
--         eKey <- B.fromAddHandler addHandlerKey
--         eHello <- B.fromAddHandler addHandlerHello
--         eMouseData <- B.fromAddHandler addHandlerMouseData

--         let eClose :: B.Event (IO ())
--             eClose = flip G.setWindowShouldClose True <$> eShouldClose

--             eRender :: B.Event (IO ())
--             eRender = B.apply ((\gs w -> do
--                                    render gs prog texSampleLoc vao (snd ebuf) tex
--                                    G.swapBuffers w) <$> bWorld) eTick

--             eEscapeToClose :: B.Event (IO ())
--             eEscapeToClose = (\(w, _b, _, _, _) -> G.setWindowShouldClose w True)
--               <$> B.filterE (\(_, k, _, _, _) -> k == G.Key'Escape) eKey

--             ePrintHello :: B.Event (IO ())
--             ePrintHello = const (print ("hello" :: String)) <$> eHello

--         movementScript <- liftIO $ scriptLoad $ 
--                           ScriptName "scripts/" "Movement"
--         gameState <- scriptInstall movementScript 
--                      (initGameState & gameStateMousePosEvent .~ eMouseData)

--         eWorld <- B.accumE (pure initGameState) (B.unions $
--                                                  fmap (fmap (=<<)) $
--                                                  toList $
--                                                  gameState ^. gameStateEndoRegister . unEndoRegister)
--         eWorld' <- B.execute eWorld
--         bWorld <- B.stepper initGameState eWorld'

--         -- bWorld <- B.stepper gameState eNewWorld
--         -- let eWorld = (const <$> bWorld) B.<@> eTick
--         -- let eUpdater = (\w -> B.unions $
--         --                       fmap (fmap (>=>)) $
--         --                       toList $
--         --                       w ^. gameStateEndoRegister . unEndoRegister) <$> eWorld
--         -- eUpdater' <- B.switchE eUpdater
--         -- bUpdater <- B.accumB return eUpdater'
--         -- let eUpdateWorld = bUpdater B.<@> eWorld
--         -- eNewWorld <- B.execute eUpdateWorld

--         B.reactimate ePrintHello
--         B.reactimate eClose
--         B.reactimate eRender
--         B.reactimate eEscapeToClose

--   net <- liftIO $ B.compile network
--   liftIO $ B.actuate net
--   return (hello, shouldClose, key, mouseData, tick)

-- installKeyEventListener :: ((G.Window, G.Key, ScanCode, G.KeyState, G.ModifierKeys) -> GameState -> B.MomentIO GameState)
--                         -> EndoName
--                         -> GameState
--                         -> B.MomentIO GameState
-- installKeyEventListener el en gs =
--   return $ gs & gameStateEndoRegister %~ registerEndo en (el <$> (gs ^. gameStateKeyEvent))


-- installMouseEventListener :: (MousePos -> GameState -> B.MomentIO GameState) -> EndoName -> GameState -> B.MomentIO GameState
-- installMouseEventListener el en gs =
--   return $ gs & gameStateEndoRegister %~ registerEndo en (el <$> (gs ^. gameStateMousePosEvent))
