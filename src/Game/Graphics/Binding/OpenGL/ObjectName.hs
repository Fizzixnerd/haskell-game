module Game.Graphics.Binding.OpenGL.ObjectName where

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
  genObjectName_ :: IO a
  genObjectNames_ :: Int -> IO (Vector a)

  genObjectName_ = VS.head <$> genObjectNames_ 1
  genObjectNames_ n = replicateM n genObjectName_

genObjectName :: (GeneratableObjectName a, MonadIO m) => m a
genObjectName = liftIO genObjectName_
genObjectNames :: (GeneratableObjectName a, MonadIO m) => Int -> m (Vector a)
genObjectNames n = liftIO (genObjectNames_ n)

