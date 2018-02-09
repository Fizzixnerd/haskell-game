{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- {-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}

module Game.StorableTypes where

-- import GHC.Generics
-- import Foreign.Storable.Generic
-- import Foreign.C.Types
-- import Control.Lens
-- import qualified Linear as L

-- instance (GStorable a) => GStorable (L.V2 a)
-- instance (GStorable a) => GStorable (L.V3 a)
-- instance (GStorable a) => GStorable (L.V4 a)

-- data VTNPoint = VTNPoint
--   { _vtnPointV :: !(L.V4 CFloat)
--   , _vtnPointT :: !(L.V2 CFloat)
--   , _vtnPointN :: !(L.V3 CFloat)
--   } deriving (Eq, Show, Ord, Read, Generic, GStorable)

-- data VTNIndex = VTNIndex
--   { _vtnIndexV :: !CUShort
--   , _vtnIndexT :: !CUShort
--   , _vtnIndexN :: !CUShort
--   } deriving (Eq, Show, Ord, Read, Generic, GStorable)

-- mconcat <$> mapM makeLenses [''VTNPoint]
