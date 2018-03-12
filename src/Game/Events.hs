{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Events where

import           Control.Arrow
import           Control.Wire
import qualified FRP.Netwire.Input as N
import           Foreign.C.Types
import           ClassyPrelude
import           Control.Lens
import           Game.Types
import           Game.Graphics.Types
import           Game.Graphics.Text
import           Game.Entity.Player
import           Game.Entity.Camera
import qualified Physics.Bullet as P
import           Graphics.Binding
import qualified Linear as L
import qualified Sound.OpenAL as AL
import           Game.Wires

zoomCameraWire :: GameEffectWire s
zoomCameraWire = effectWire $ do
  cam <- use gameStateCamera
  disp <- getCameraDisplacementFromTarget cam
  let dist = L.norm disp
      rs   = (\(CFloat x) -> x) (cam ^. cameraPreferredDistance) - dist
  t <- liftIO $ P.coAllocateWorldTransform $ cam ^. cameraController
  (x, y, z) <- liftIO $ P.getOrigin t
  liftIO $ P.del t
  AL.listenerPosition AL.$= AL.Vertex3 x y z
  srad <- getCameraRadialSpeed cam
  setCameraRadialForce cam (10 * rs - srad)
  sazi <- getCameraAzimuthalSpeed cam
  setCameraAzimuthalForce cam  (- sazi)
  spol <- getCameraPolarSpeed cam
  setCameraPolarForce cam (- spol)

-- turnPlayer :: GameEffectWire s
-- turnPlayer = effectWire $ do
--   p <- use gameStatePlayer
--   c <- use gameStateCamera
--   o <- getCameraOrientation c
--   setPlayerOrientation p o

rotateCamera :: MonadIO m => (Float, Float) -> Camera s -> m ()
rotateCamera (dhor, dver) cam = do
  costheta <- getCameraInclinationCos cam
  setCameraPolarForce cam (clampy costheta)
  setCameraAzimuthalForce cam dhor
  where
    clampy theta
      | theta > 0.9    = max 0 dver
      | theta < (-0.9) = min 0 dver
      | otherwise      = dver

cameraWire :: GameEffectWire s
cameraWire = camWire
  where
    rotCam = mkGen_ $ \(x, y) -> Right <$> do
      cam <- use gameStateCamera
      rotateCamera (-x * 100, -y * 100) cam
    camWire = passWire $ rotCam <<< N.mouseMickies

movePlayer :: GameWire s (L.V3 Float) ()
movePlayer = mkGen_ $ \dir -> Right <$> do
  p <- use gameStatePlayer
  let impulse = 100 * dir
  playerApplyForce p impulse

dampPlayerWire :: GameEffectWire s
dampPlayerWire = effectWire $ do
  p <- use gameStatePlayer
  v <- getPlayerLinearVelocity p
  let drag = - v
  playerApplyForce p drag

playerHorizontalMovement :: GameWire s a (L.V3 Float)
playerHorizontalMovement = (\v -> 0.1 * recip (L.norm v) L.*^ v) <$> solderWire (+) zwire xwire
  where
    fwd = mkConstM $ join $ uses gameStateCamera getCameraForward
    rgt = mkConstM $ join $ uses gameStateCamera getCameraLeft
    xwire = xorWire (keyA >>> negate <$> rgt) (keyD >>> rgt)
    zwire = xorWire (keyW >>> fwd) (keyS >>> (negate <$> fwd))

close :: GameEffectWire s
close = cls <<< keyEsc
  where
    cls = effectWire $ gameStateShouldClose .= True

jump :: GameEffectWire s
jump = jmp <<< keySpace
  where
    jmp = effectWire $ do
      p <- use gameStatePlayer
      playerJump p

basicEventStream :: (Fractional a, HasTime t s) => GameWire s a (Event a)
basicEventStream = periodic 4 . timeF

-- * Typing

keyEitherShift :: GameWire s a a
keyEitherShift = N.keyPressed Key'LeftShift <|> N.keyPressed Key'RightShift

devConsoleWriteWire :: GameWire s a a
devConsoleWriteWire = concatA $ fromList $ makeKeyWire <$>
  [ (keydebApostrophe, '\'', '\"')
  , (keydebComma, ',', '<')
  , (keydebSpace, ' ', ' ')
  , (keydebMinus, '-', '_')
  , (keydebPeriod, '.', '>')
  , (keydebSlash, '/', '?')
  , (keydebRightBracket, ']', '}')
  , (keydebLeftBracket, '[', '{')
  , (keydeb0, '0', ')')
  , (keydeb1, '1', '!')
  , (keydeb2, '2', '@')
  , (keydeb3, '3', '#')
  , (keydeb4, '4', '$')
  , (keydeb5, '5', '%')
  , (keydeb6, '6', '^')
  , (keydeb7, '7', '&')
  , (keydeb8, '8', '*')
  , (keydeb9, '9', '(')
  , (keydebSemicolon, ';', ':')
  , (keydebEqual, '=', '+')
  , (keydebA, 'a', 'A')
  , (keydebB, 'b', 'B')
  , (keydebC, 'c', 'C')
  , (keydebD, 'd', 'D')
  , (keydebE, 'e', 'E')
  , (keydebF, 'f', 'F')
  , (keydebG, 'g', 'G')
  , (keydebH, 'h', 'H')
  , (keydebI, 'i', 'I')
  , (keydebJ, 'j', 'J')
  , (keydebK, 'k', 'K')
  , (keydebL, 'l', 'L')
  , (keydebM, 'm', 'M')
  , (keydebN, 'n', 'N')
  , (keydebO, 'o', 'O')
  , (keydebP, 'p', 'P')
  , (keydebQ, 'q', 'Q')
  , (keydebR, 'r', 'R')
  , (keydebS, 's', 'S')
  , (keydebT, 't', 'T')
  , (keydebU, 'u', 'U')
  , (keydebV, 'v', 'V')
  , (keydebW, 'w', 'W')
  , (keydebX, 'x', 'X')
  , (keydebY, 'y', 'Y')
  , (keydebZ, 'z', 'Z')
  , (keydebBackslash, '\\', '|')
  ]
  where
    snocWire ch = mkConstM $ gameStateDevConsole . _Just %= snocTextBuffer ch
    makeKeyWire (wire, noShiftKey, shiftKey) = passWire $ wire >>> ((keyEitherShift >>> snocWire shiftKey) <|> snocWire noShiftKey)

devConsoleToggleWire :: GameEffectWire s
devConsoleToggleWire = keydebGrave >>> toggle_
  where
    toggle_ = effectWire $ do
      gis <- use gameStateKeyboardInputScheme
      case gis of
        InputPlaying    -> gameStateDevConsole .= Just initDevConsole >> gameStateKeyboardInputScheme .= InputDevConsole
        InputDevConsole -> gameStateDevConsole .= Nothing >> gameStateKeyboardInputScheme .= InputPlaying

devConsoleDelWire :: GameEffectWire s
devConsoleDelWire = passWire $ keydebDelete >>> mkConstM (gameStateDevConsole . _Just %= delOffTextBuffer 1)

executeBufferWire :: GameEffectWire s
executeBufferWire = keydebEnter >>> effectWire (executeTextBufferWith print)
