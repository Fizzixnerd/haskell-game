module Game.Graphics.OpenGL.ObjectName where

import Foreign.Storable
import Control.Monad.IO.Class
import Data.Vector.Storable as VS

class Storable a => ObjectName a where
  isObjectName :: MonadIO m => a -> m Bool
  deleteObjectName :: MonadIO m => a -> m ()
  deleteObjectNames :: MonadIO m => Vector a -> m ()

  deleteObjectName = deleteObjectNames . singleton
  deleteObjectNames = VS.mapM_ deleteObjectName

class ObjectName a => GeneratableObjectName a where
  genObjectName :: MonadIO m => m a
  genObjectNames :: MonadIO m => Int -> m (Vector a)

  genObjectName = VS.head <$> genObjectNames 1
  genObjectNames n = replicateM n genObjectName

