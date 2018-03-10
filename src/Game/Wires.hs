{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Game.Wires where

import           ClassyPrelude
import           Control.Wire
import qualified FRP.Netwire.Input as N
import           Game.Types
import           Graphics.Binding
import           Data.Either (isLeft)

-- * Wire utilities

-- | Is the identity if only one wire is producing. If both are, it
-- merges the results with merge. This steps both wires.
solderWire :: (Monoid e, Monad m) => (b -> b -> b) -> Wire s e m a b -> Wire s e m a b -> Wire s e m a b
solderWire merge' w1 w2 = WGen $ \s eea -> do
  (eeb1, w1') <- stepWire w1 s eea
  (eeb2, w2') <- stepWire w2 s eea
  let res = merger eeb1 eeb2
  res `seq` return (res, solderWire merge' w1' w2')
  where
    merger = \case
      Left err -> left (mappend err)
      Right x  -> const (Right x) ||| (Right . merge' x)

-- | If both are producing or both are inhibited, then it inhibits.
-- Otherwise it acts like the producing one. This thing has to step
-- both wires.
solderWireM :: (Monoid e, Monad m) => (b -> b -> m b) -> Wire s e m a b -> Wire s e m a b -> Wire s e m a b
solderWireM merge' w1 w2 = WGen $ \s eea -> do
  (eeb1, w1') <- stepWire w1 s eea
  (eeb2, w2') <- stepWire w2 s eea
  res <- merger eeb1 eeb2
  res `seq` return (res, solderWireM merge' w1' w2')
  where
    merger = \case
      Left err -> pure . left (mappend err)
      Right x  -> const (pure . Right $ x) ||| (fmap Right . merge' x)

-- | An identity wire that executes a monadic action when activated,
-- and passes the input through unchanged
effectWire :: Monad m => m c -> Wire s e m a a
effectWire act = mkGen_ $ \a -> void act >> return (Right a)

-- | A constant wire that executes a monadic action to obtain its output.
mkConstM :: Monad m => m b -> Wire s e m a b
mkConstM act = mkGen_ $ const $ Right <$> act

-- | A wire transformer that executes the given wire, discards the result, and passes its input through unchanged.
passWire :: Monad m => Wire s e m a b -> Wire s e m a a
passWire wire = (wire &&& id) >>> arr snd

-- A stepper wire that hopefully works.
-- this may be silly

eitherWire :: Monad m => b -> Wire s e m a b -> Wire s e m a b
eitherWire def wire = WGen $ \s -> either (const $ return $ lstrict (Right def, eitherWire def wire))
                                          (\v -> do
                                              (eeb, wire') <- stepWire wire s (Right v)
                                              return $ lstrict (either (const (Right def)) Right eeb, wire')
                                          )

stepperWire :: Monad m => Wire s e m a a
stepperWire = WPure $ \_ -> \case
  l@(Left _) -> (l, stepperWire)
  (Right a)  -> (Right a, stepperWire' a)
    where
      stepperWire' x = eitherWire x $ mkPureN (Right &&& stepperWire')

-- If both are producing or both are inhibited, then it inhibits.
-- Otherwise it acts like the producing one.
-- This thing has to step both wires.

xorWire :: (Monoid e, Monad m) => Wire s e m a b -> Wire s e m a b -> Wire s e m a b
xorWire w1 w2 = WGen $ \s eea -> do
  (eeb1, w1') <- stepWire w1 s eea
  (eeb2, w2') <- stepWire w2 s eea
  return $ lstrict (inverter eeb1 eeb2, xorWire w1' w2')
  where
    inverter = \case
      Left err -> left (mappend err)
      Right x  -> const (Right x) ||| const (Left mempty)

stateSwitchingWire :: (Monoid s, Monad m) => m a -> (a -> Wire s e m c d) -> Wire s e m c d
stateSwitchingWire act f = WGen $ \s eec -> do
  a <- act
  (eed, _) <- stepWire (f a) s eec
  return $ lstrict (eed, stateSwitchingWire act f)

-- * Input state wires

-- ** Various alphanumeric keys
keyApostrophe :: GameWire s a a
keyApostrophe = N.keyPressed Key'Apostrophe

keyComma :: GameWire s a a
keyComma = N.keyPressed Key'Comma

keyMinus :: GameWire s a a
keyMinus = N.keyPressed Key'Minus

keyPeriod :: GameWire s a a
keyPeriod = N.keyPressed Key'Period

keySlash :: GameWire s a a
keySlash = N.keyPressed Key'Slash

keyRightBracket :: GameWire s a a
keyRightBracket = N.keyPressed Key'RightBracket

keyLeftBracket :: GameWire s a a
keyLeftBracket = N.keyPressed Key'LeftBracket

key0 :: GameWire s a a
key0 = N.keyPressed Key'0

key1 :: GameWire s a a
key1 = N.keyPressed Key'1

key2 :: GameWire s a a
key2 = N.keyPressed Key'2

key3 :: GameWire s a a
key3 = N.keyPressed Key'3

key4 :: GameWire s a a
key4 = N.keyPressed Key'4

key5 :: GameWire s a a
key5 = N.keyPressed Key'5

key6 :: GameWire s a a
key6 = N.keyPressed Key'6

key7 :: GameWire s a a
key7 = N.keyPressed Key'7

key8 :: GameWire s a a
key8 = N.keyPressed Key'8

key9 :: GameWire s a a
key9 = N.keyPressed Key'9

keySemicolon :: GameWire s a a
keySemicolon = N.keyPressed Key'Semicolon

keyEqual :: GameWire s a a
keyEqual = N.keyPressed Key'Equal

keyA :: GameWire s a a
keyA = N.keyPressed Key'A

keyB :: GameWire s a a
keyB = N.keyPressed Key'B

keyC :: GameWire s a a
keyC = N.keyPressed Key'C

keyD :: GameWire s a a
keyD = N.keyPressed Key'D

keyE :: GameWire s a a
keyE = N.keyPressed Key'E

keyF :: GameWire s a a
keyF = N.keyPressed Key'F

keyG :: GameWire s a a
keyG = N.keyPressed Key'G

keyH :: GameWire s a a
keyH = N.keyPressed Key'H

keyI :: GameWire s a a
keyI = N.keyPressed Key'I

keyJ :: GameWire s a a
keyJ = N.keyPressed Key'J

keyK :: GameWire s a a
keyK = N.keyPressed Key'K

keyL :: GameWire s a a
keyL = N.keyPressed Key'L

keyM :: GameWire s a a
keyM = N.keyPressed Key'M

keyN :: GameWire s a a
keyN = N.keyPressed Key'N

keyO :: GameWire s a a
keyO = N.keyPressed Key'O

keyP :: GameWire s a a
keyP = N.keyPressed Key'P

keyQ :: GameWire s a a
keyQ = N.keyPressed Key'Q

keyR :: GameWire s a a
keyR = N.keyPressed Key'R

keyS :: GameWire s a a
keyS = N.keyPressed Key'S

keyT :: GameWire s a a
keyT = N.keyPressed Key'T

keyU :: GameWire s a a
keyU = N.keyPressed Key'U

keyV :: GameWire s a a
keyV = N.keyPressed Key'V

keyW :: GameWire s a a
keyW = N.keyPressed Key'W

keyX :: GameWire s a a
keyX = N.keyPressed Key'X

keyY :: GameWire s a a
keyY = N.keyPressed Key'Y

keyZ :: GameWire s a a
keyZ = N.keyPressed Key'Z

keyBackslash :: GameWire s a a
keyBackslash = N.keyPressed Key'Backslash

keyGrave :: GameWire s a a
keyGrave = N.keyPressed Key'GraveAccent

-- ** White space keys
keySpace :: GameWire s a a
keySpace = N.keyPressed Key'Space

keyTab :: GameWire s a a
keyTab = N.keyPressed Key'Tab

keyEnter :: GameWire s a a
keyEnter = N.keyPressed Key'Enter

-- ** Other keys
keyEsc :: GameWire s a a
keyEsc = N.keyPressed Key'Escape

-- ** Mouse buttons
mouseL :: GameWire s a a
mouseL = N.mousePressed MouseButton'1

mouseR :: GameWire s a a
mouseR = N.mousePressed MouseButton'2
