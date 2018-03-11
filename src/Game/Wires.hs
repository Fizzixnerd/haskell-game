{-# LANGUAGE FlexibleContexts #-}
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

-- * Wire utilities

unEvent :: Monad m => Wire s e m a (Event a) -> Wire s e m a a
unEvent eventWire = passWire eventWire

concatA :: ArrowPlus a => Vector (a b b) -> a b b
concatA = foldr (<+>) id

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

-- | Lift a monad-valued function to a wire
mkLiftM_ :: Monad m => (a -> m b) -> Wire s e m a b
mkLiftM_ act = mkGen_ $ fmap Right . act

-- | A constant wire that executes a monadic action to obtain its output.
mkConstM :: Monad m => m b -> Wire s e m a b
mkConstM = mkLiftM_ . const

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

keyDelete :: GameWire s a a
keyDelete = N.keyPressed Key'Delete

-- ** Mouse buttons
mouseL :: GameWire s a a
mouseL = N.mousePressed MouseButton'1

mouseR :: GameWire s a a
mouseR = N.mousePressed MouseButton'2
-- * Key input wires - debounce versions.

-- ** Various alphanumeric keys
keydebApostrophe :: GameWire s a a
keydebApostrophe = N.keyDebounced Key'Apostrophe

keydebComma :: GameWire s a a
keydebComma = N.keyDebounced Key'Comma

keydebMinus :: GameWire s a a
keydebMinus = N.keyDebounced Key'Minus

keydebPeriod :: GameWire s a a
keydebPeriod = N.keyDebounced Key'Period

keydebSlash :: GameWire s a a
keydebSlash = N.keyDebounced Key'Slash

keydebRightBracket :: GameWire s a a
keydebRightBracket = N.keyDebounced Key'RightBracket

keydebLeftBracket :: GameWire s a a
keydebLeftBracket = N.keyDebounced Key'LeftBracket

keydeb0 :: GameWire s a a
keydeb0 = N.keyDebounced Key'0

keydeb1 :: GameWire s a a
keydeb1 = N.keyDebounced Key'1

keydeb2 :: GameWire s a a
keydeb2 = N.keyDebounced Key'2

keydeb3 :: GameWire s a a
keydeb3 = N.keyDebounced Key'3

keydeb4 :: GameWire s a a
keydeb4 = N.keyDebounced Key'4

keydeb5 :: GameWire s a a
keydeb5 = N.keyDebounced Key'5

keydeb6 :: GameWire s a a
keydeb6 = N.keyDebounced Key'6

keydeb7 :: GameWire s a a
keydeb7 = N.keyDebounced Key'7

keydeb8 :: GameWire s a a
keydeb8 = N.keyDebounced Key'8

keydeb9 :: GameWire s a a
keydeb9 = N.keyDebounced Key'9

keydebSemicolon :: GameWire s a a
keydebSemicolon = N.keyDebounced Key'Semicolon

keydebEqual :: GameWire s a a
keydebEqual = N.keyDebounced Key'Equal

keydebA :: GameWire s a a
keydebA = N.keyDebounced Key'A

keydebB :: GameWire s a a
keydebB = N.keyDebounced Key'B

keydebC :: GameWire s a a
keydebC = N.keyDebounced Key'C

keydebD :: GameWire s a a
keydebD = N.keyDebounced Key'D

keydebE :: GameWire s a a
keydebE = N.keyDebounced Key'E

keydebF :: GameWire s a a
keydebF = N.keyDebounced Key'F

keydebG :: GameWire s a a
keydebG = N.keyDebounced Key'G

keydebH :: GameWire s a a
keydebH = N.keyDebounced Key'H

keydebI :: GameWire s a a
keydebI = N.keyDebounced Key'I

keydebJ :: GameWire s a a
keydebJ = N.keyDebounced Key'J

keydebK :: GameWire s a a
keydebK = N.keyDebounced Key'K

keydebL :: GameWire s a a
keydebL = N.keyDebounced Key'L

keydebM :: GameWire s a a
keydebM = N.keyDebounced Key'M

keydebN :: GameWire s a a
keydebN = N.keyDebounced Key'N

keydebO :: GameWire s a a
keydebO = N.keyDebounced Key'O

keydebP :: GameWire s a a
keydebP = N.keyDebounced Key'P

keydebQ :: GameWire s a a
keydebQ = N.keyDebounced Key'Q

keydebR :: GameWire s a a
keydebR = N.keyDebounced Key'R

keydebS :: GameWire s a a
keydebS = N.keyDebounced Key'S

keydebT :: GameWire s a a
keydebT = N.keyDebounced Key'T

keydebU :: GameWire s a a
keydebU = N.keyDebounced Key'U

keydebV :: GameWire s a a
keydebV = N.keyDebounced Key'V

keydebW :: GameWire s a a
keydebW = N.keyDebounced Key'W

keydebX :: GameWire s a a
keydebX = N.keyDebounced Key'X

keydebY :: GameWire s a a
keydebY = N.keyDebounced Key'Y

keydebZ :: GameWire s a a
keydebZ = N.keyDebounced Key'Z

keydebBackslash :: GameWire s a a
keydebBackslash = N.keyDebounced Key'Backslash

keydebGrave :: GameWire s a a
keydebGrave = N.keyDebounced Key'GraveAccent

-- ** White space keydebs
keydebSpace :: GameWire s a a
keydebSpace = N.keyDebounced Key'Space

keydebTab :: GameWire s a a
keydebTab = N.keyDebounced Key'Tab

keydebEnter :: GameWire s a a
keydebEnter = N.keyDebounced Key'Enter

-- ** Other keydebs
keydebEsc :: GameWire s a a
keydebEsc = N.keyDebounced Key'Escape

keydebDelete :: GameWire s a a
keydebDelete = N.keyDebounced Key'Delete
