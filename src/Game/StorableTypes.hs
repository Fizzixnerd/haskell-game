{-# LANGUAGE TemplateHaskell #-}

module Game.StorableTypes where

import Foreign.C.Types
import Foreign.Storable
import Control.Lens
import qualified Linear as L

data VTNPoint = VTNPoint
  { _vtnPointV :: !(L.V4 CFloat)
  , _vtnPointT :: !(L.V2 CFloat)
  , _vtnPointN :: !(L.V3 CFloat)
  } deriving (Eq, Show, Ord, Read)

data VTNIndex = VTNIndex
  { _vtnIndexV :: !Int
  , _vtnIndexT :: !Int
  , _vtnIndexN :: !Int
  } deriving (Eq, Show, Ord, Read)

instance Storable VTNPoint where
  sizeOf _ = 9 * sizeOf (0 :: CFloat)
  alignment _ = alignment (undefined :: CFloat)
  poke ptr (VTNPoint v t n) = do
    pokeByteOff ptr 0 v
    pokeByteOff ptr (4 * sizeOf (0 :: CFloat)) t
    pokeByteOff ptr (6 * sizeOf (0 :: CFloat)) n
  peek ptr = do
    v <- peekByteOff ptr 0
    t <- peekByteOff ptr (4 * sizeOf (0 :: CFloat))
    n <- peekByteOff ptr (6 * sizeOf (0 :: CFloat))
    return $ VTNPoint v t n

instance Storable VTNIndex where
  sizeOf _ = 3 * sizeOf (0 :: Int)
  alignment _ = alignment (undefined :: Int)
  poke ptr (VTNIndex v t n) = do
    pokeByteOff ptr 0 v
    pokeByteOff ptr (1 * sizeOf (0 :: Int)) t
    pokeByteOff ptr (2 * sizeOf (0 :: Int)) n
  peek ptr = do
    v <- peekByteOff ptr 0
    t <- peekByteOff ptr (1 * sizeOf (0 :: Int))
    n <- peekByteOff ptr (2 * sizeOf (0 :: Int))
    return $ VTNIndex v t n

mconcat <$> mapM makeLenses [''VTNPoint, ''VTNIndex]
