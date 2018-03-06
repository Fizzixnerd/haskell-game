module Graphics.Binding.OpenGL.Shader where

import Data.ByteString
import Data.StateVar
import Graphics.Binding.OpenGL.Types
import Graphics.Binding.OpenGL.Utils
import Graphics.GL.Core45

compileShader :: (Shader t, MonadIO m) => t -> m (Maybe ByteString)
compileShader t = do
  glCompileShader n
  status <- unmarshalGLboolean <$> foreignPoke (glGetShaderiv n GL_COMPILE_STATUS)
  if status
    then return Nothing
    else Just <$> withForeignBufferBS (glGetShaderiv n GL_INFO_LOG_LENGTH) (glGetShaderInfoLog n)
  where
    n = marshalShaderObject t

shaderDeleteStatus :: (Shader t, MonadIO m) => t -> m Bool
shaderDeleteStatus shader = unmarshalGLboolean <$> foreignPoke (glGetShaderiv (marshalShaderObject shader) GL_DELETE_STATUS)

shaderInfoLog :: (Shader t, MonadIO m) => t -> m ByteString
shaderInfoLog shader = withForeignBufferBS (glGetShaderiv n GL_INFO_LOG_LENGTH) (glGetShaderInfoLog n)
  where
    n = marshalShaderObject shader

shaderSource :: Shader t => t -> StateVar ByteString
shaderSource shader = makeStateVar getSource setSource
    where
      n = marshalShaderObject shader
      getSource = withForeignBufferBS (glGetShaderiv n GL_SHADER_SOURCE_LENGTH) (glGetShaderSource n)
      setSource src =
        withByteString src $ \srcPtr srcLength ->
        with srcPtr $ \srcPtrBuf ->
                        with srcLength $ \srcLengthBuf ->
                                           glShaderSource n 1 srcPtrBuf srcLengthBuf
