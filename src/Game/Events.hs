{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Events where

import           ClassyPrelude
import           Control.Lens
import           Game.Graphics.Rendering
import           Game.Types
import           GHC.Float (double2Float)
import qualified Graphics.Rendering.OpenGL.GL as G
import qualified Graphics.UI.GLFW             as G
import qualified Linear as L
import           Plugin.Load
import qualified Reactive.Banana.Combinators  as B
import qualified Reactive.Banana.Frameworks   as B

keyToMovement :: G.Key -> Maybe Movement
keyToMovement G.Key'W = Just MoveForward
keyToMovement G.Key'A = Just MoveLeft
keyToMovement G.Key'S = Just MoveBackward
keyToMovement G.Key'D = Just MoveRight
keyToMovement G.Key'LeftShift = Just MoveDown
keyToMovement G.Key'Space = Just MoveUp
keyToMovement _ = Nothing

mouseToMovement :: (G.Window, Double, Double) -> Movement
mouseToMovement (_, x, y) = MoveCameraDir x y

moveCamera :: Float -> Float -> Movement -> Camera -> Camera
moveCamera _ t MoveLeft            = translateCameraRelative (L.V3 (negate t) 0 0)
moveCamera _ t MoveRight           = translateCameraRelative (L.V3 t 0 0)
moveCamera _ t MoveForward         = translateCameraRelative (L.V3 0 0 (negate t))
moveCamera _ t MoveBackward        = translateCameraRelative (L.V3 0 0 t)
moveCamera _ t MoveUp              = translateCameraRelative (L.V3 0 t 0)
moveCamera _ t MoveDown            = translateCameraRelative (L.V3 0 (negate t) 0)
moveCamera o _ (MoveCameraDir x y) = rotateCamera q
  where
    q = mousePosToRot o x y

rotateCamera :: (Float, Float) -> Camera -> Camera
rotateCamera (dhor, dver) cam = cam & cameraOrientation %~ go
  where
    go (hor, ver) = (hor + dhor, max (-pi/2) . min (pi/2) $ ver + dver)

translateCameraRelative :: L.V3 Float -> Camera -> Camera
translateCameraRelative v cam = cam & cameraPosition +~ vrel
  where
    vrel = L.rotate (L.axisAngle (L.V3 0 1 0) (fst . _cameraOrientation $ cam)) v

mousePosToRot :: Float -> Double -> Double -> (Float, Float)
mousePosToRot mouseSpeed x y = (xrot, yrot)
  where
    xrot = mouseSpeed * double2Float (1920/2 - x)
    yrot = mouseSpeed * double2Float (1080/2 - y)

compileGameNetwork ::
  MonadIO m =>
  G.Program
  -> G.UniformLocation
  -> G.UniformLocation
  -> G.VertexArrayObject
  -> (G.BufferObject, Int)
  -> G.TextureObject
  -> m (NamedHandler b1, NamedHandler b2, NamedHandler G.Window,
        NamedHandler (G.Window, G.Key, c, d, e),
        NamedHandler (G.Window, Double, Double), NamedHandler G.Window,
        NamedHandler b3)
compileGameNetwork prog mvpLoc texSampleLoc vao ebuf tex = do
  -- get the Handlers we need.
  (addHandlerShouldClose, shouldClose) <- newNamedEventHandler "shouldClose"
  (addHandlerTick, tick) <- newNamedEventHandler "tick"
  (addHandlerKey, key) <- newNamedEventHandler "key"
  (addHandlerHello, hello) <- newNamedEventHandler "hello"
  (addHandlerMousePos, mousePos) <- newNamedEventHandler "mousePos"
  (addHandlerGameReset, gameReset) <- newNamedEventHandler "gameReset"
  (addHandlerFuckWithFoV, fuckWithFoV) <- newNamedEventHandler "fuckWithFoV"

  (foreignFriend :: GameState -> GameState) <- liftIO $ loadPlugin "scripts" "ForeignEvent" ("gs" :: String)

  let network :: B.MomentIO ()
      network = mdo
        eTick <- B.fromAddHandler addHandlerTick
        eShouldClose <- B.fromAddHandler addHandlerShouldClose
        eKey <- B.fromAddHandler addHandlerKey
        eHello <- B.fromAddHandler addHandlerHello
        eMousePos <- B.fromAddHandler addHandlerMousePos
        eGameReset <- B.fromAddHandler addHandlerGameReset
        eFuckWithFoV <- B.fromAddHandler addHandlerFuckWithFoV


        B.reactimate ePrintHello
        B.reactimate eClose
        B.reactimate eRender
        B.reactimate eEscapeToClose

        let eClose :: B.Event (IO ())
            eClose = flip G.setWindowShouldClose True <$> eShouldClose

            eRender :: B.Event (IO ())
            eRender = B.apply ((\gs w -> do
                                   render gs prog mvpLoc texSampleLoc vao ebuf tex
                                   G.swapBuffers w) <$> bWorld) eTick

            eEscapeToClose :: B.Event (IO ())
            eEscapeToClose = (\(w, _, _, _, _) -> G.setWindowShouldClose w True)
              <$> B.filterE (\(_, k, _, _, _) -> k == G.Key'Escape) eKey

            ePrintHello :: B.Event (IO ())
            ePrintHello = const (print ("hello" :: String)) <$> eHello

            eMovement :: B.Event Movement
            eMovement = B.unionWith const (B.filterJust ((\(_,k,_,_,_) -> keyToMovement k) <$> eKey)) (mouseToMovement <$> eMousePos)

            eCamMove :: B.Event (GameState -> GameState)
            eCamMove = (\m gs -> (gameStateCamera %~ moveCamera (0.005/60) (3/60) m $ gs)) <$> eMovement

            eResetGame :: B.Event (GameState -> GameState)
            eResetGame = const (const initGameState) <$> eGameReset

            eFoVFuckery :: B.Event (GameState -> GameState)
            eFoVFuckery = const foreignFriend <$> eFuckWithFoV

            eFoVFuckeryYelling :: B.Event (IO ())
            eFoVFuckeryYelling = const (print ("FUCK ME" :: Text)) <$> eFuckWithFoV

        bWorld <- B.accumB initGameState (B.unions [eCamMove, eResetGame, eFoVFuckery])

        B.reactimate ePrintHello
        B.reactimate eClose
        B.reactimate eRender
        B.reactimate eEscapeToClose
        B.reactimate eFoVFuckeryYelling

  net <- liftIO $ B.compile network
  liftIO $ B.actuate net
  return (hello, gameReset, shouldClose, key, mousePos, tick, fuckWithFoV)
