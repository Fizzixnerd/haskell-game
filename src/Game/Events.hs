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
import           Game.Entity.Player
import           Game.Entity.Camera
import qualified Physics.Bullet as P
import           Graphics.Binding
import qualified Linear as L
import qualified Sound.OpenAL as AL
import           Game.Wires
import qualified Data.Text as T

zoomCamera :: GameEffectWire s
zoomCamera = effectWire $ do
  cam <- use gameStateCamera
  disp <- getCameraDisplacementFromTarget cam
  let dist = L.norm disp
      rs   = (cam ^. cameraPreferredDistance) - dist
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
  --setCameraPolarSpeed cam (clampy costheta)
  setCameraAzimuthalForce cam (CFloat dhor)
  where
    clampy theta
      | theta > 0.9    = CFloat $ max 0 dver
      | theta < (-0.9) = CFloat $ min 0 dver
      | otherwise      = CFloat dver

camera :: GameEffectWire s
camera = camWire
  where
    rotCam = mkGen_ $ \(x, y) -> Right <$> do
      cam <- use gameStateCamera
      rotateCamera (-x * 100, -y * 100) cam
    camWire = passWire $ rotCam <<< N.mouseMickies

movePlayer :: GameWire s (L.V3 CFloat) ()
movePlayer = mkGen_ $ \dir -> Right <$> do
  p <- use gameStatePlayer
  c <- use gameStateCamera
  let force_ = 100 * dir
  playerApplyForce p force_
  phiHat <- getCameraPhiHat c
  cameraApplyAzimuthalForce c (L.dot force_ phiHat)

playerHorizontalMovement :: GameWire s a (L.V3 CFloat)
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

keydebGrave :: GameWire s a a
keydebGrave = N.keyDebounced Key'GraveAccent

keyEitherShift :: GameWire s a a
keyEitherShift = N.keyPressed Key'LeftShift <|> N.keyPressed Key'RightShift

allTextWire :: GameWire s a Text
allTextWire = mconcat . fmap (fmap T.singleton . makeKeyWire) $
  [ (keyApostrophe, '\'', '\"')
  , (keyComma, ',', '<')
  , (keyMinus, '-', '_')
  , (keyPeriod, '.', '>')
  , (keySlash, '/', '?')
  , (keyRightBracket, ']', '}')
  , (keyLeftBracket, '[', '{')
  , (key0, '0', ')')
  , (key1, '1', '!')
  , (key2, '2', '@')
  , (key3, '3', '#')
  , (key4, '4', '$')
  , (key5, '5', '%')
  , (key6, '6', '^')
  , (key7, '7', '&')
  , (key8, '8', '*')
  , (key9, '9', '(')
  , (keySemicolon, ';', ':')
  , (keyEqual, '=', '+')
  , (keyA, 'a', 'A')
  , (keyB, 'b', 'B')
  , (keyC, 'c', 'C')
  , (keyD, 'd', 'D')
  , (keyE, 'e', 'E')
  , (keyF, 'f', 'F')
  , (keyG, 'g', 'G')
  , (keyH, 'h', 'H')
  , (keyI, 'i', 'I')
  , (keyJ, 'j', 'J')
  , (keyK, 'k', 'K')
  , (keyL, 'l', 'L')
  , (keyM, 'm', 'M')
  , (keyN, 'n', 'N')
  , (keyO, 'o', 'O')
  , (keyP, 'p', 'P')
  , (keyQ, 'q', 'Q')
  , (keyR, 'r', 'R')
  , (keyS, 's', 'S')
  , (keyT, 't', 'T')
  , (keyU, 'u', 'U')
  , (keyV, 'v', 'V')
  , (keyW, 'w', 'W')
  , (keyX, 'x', 'X')
  , (keyY, 'y', 'Y')
  , (keyZ, 'z', 'Z')
  , (keyBackslash, '\\', '|')
  ]
  where
    makeKeyWire (wire, noShiftKey, shiftKey) = wire >>> ((keyEitherShift >>> pure shiftKey) <|> pure noShiftKey)

devConsoleToggleWire :: GameEffectWire s
devConsoleToggleWire = keydebGrave >>> toggle_
  where
    toggle_ = effectWire $ do
      gis <- use gameStateKeyboardInputScheme
      case gis of
        InputPlaying    -> gameStateDevConsole .= Just initDevConsole >> gameStateKeyboardInputScheme .= InputDevConsole
        InputDevConsole -> gameStateDevConsole .= Nothing >> gameStateKeyboardInputScheme .= InputPlaying

devConsoleWire :: GameEffectWire s
devConsoleWire = devConsoleToggleWire
