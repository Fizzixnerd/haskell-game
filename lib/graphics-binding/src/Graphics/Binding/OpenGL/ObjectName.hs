module Graphics.Binding.OpenGL.ObjectName where

import Control.Monad.IO.Class
import Control.Monad

class ObjectName a where
  isObjectName :: MonadIO m => a -> m Bool
  deleteObjectName :: MonadIO m => a -> m ()
  deleteObjectNames :: MonadIO m => [a] -> m ()

  deleteObjectName = deleteObjectNames . (:[])
  deleteObjectNames = mapM_ deleteObjectName

class GeneratableObjectName a where
  genObjectName_ :: IO a
  genObjectNames_ :: Int -> IO [a]

  genObjectName_ = head <$> genObjectNames_ 1
  genObjectNames_ = flip replicateM genObjectName_

genObjectName :: (MonadIO m, GeneratableObjectName a) => m a
genObjectName = liftIO genObjectName_

genObjectNames :: (MonadIO m, GeneratableObjectName a) => Int -> m [a]
genObjectNames n = liftIO $ genObjectNames_ n
