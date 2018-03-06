module Graphics.Binding.OpenGL.Rendering where

import Graphics.GL.Core45
import Graphics.GL.Types
import Graphics.Binding.OpenGL.Utils
import Graphics.Binding.OpenGL.Types

clear :: MonadIO m => ClearBuffer -> m ()
clear = liftIO . glClear . marshalClearBuffer

drawElements :: MonadIO m => PrimitiveMode -> GLsizei -> IndexType -> m ()
drawElements mode count typ = glDrawElements (marshalPrimitiveMode mode) count (marshalIndexType typ) nullPtr
