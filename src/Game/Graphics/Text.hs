{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Graphics.Text where

import ClassyPrelude
import Game.Graphics.Types
import Game.Types
import Control.Lens

succOverflow :: (Eq a, Bounded a, Enum a) => a -> a
succOverflow x
  | x == maxBound = minBound
  | otherwise     = succ x

snocTextBuffer :: HasBufferText a Text => Char -> a -> a
snocTextBuffer ch = over bufferText (`ClassyPrelude.snoc` ch)

appendTextBuffer :: HasBufferText a Text => Text -> a -> a
appendTextBuffer txt = over bufferText (<> txt)

delOffTextBuffer :: HasBufferText a Text => Int -> a -> a
delOffTextBuffer n = over bufferText (take n)

-- Can use %%= here, with a lens to a text buffer.
textBufferFlush :: TextBuffer -> (Text, TextBuffer)
textBufferFlush (TextBuffer txt) = (txt, TextBuffer "")

executeTextBufferWith :: (Text -> Game s a) -> Game s a
executeTextBufferWith processor = do
  txt <- gameStateDevConsole . _Just . textBuffer %%= textBufferFlush
  processor txt
