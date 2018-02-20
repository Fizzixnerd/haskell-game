{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Game.Graphics.Binding.OpenGL.Shader where

import Data.ByteString
import Data.StateVar
import Game.Graphics.Binding.OpenGL.Boolean
import Game.Graphics.Binding.OpenGL.ObjectName
import Game.Graphics.Binding.OpenGL.Utils
import Graphics.GL.Core45
import Graphics.GL.Types

newtype VertexShader = VertexShader { _vertexShaderGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)
instance ObjectName VertexShader where
  isObjectName (VertexShader n) = unmarshallGLboolean <$> glIsShader n
  deleteObjectName (VertexShader n) = glDeleteShader n
instance GeneratableObjectName VertexShader where
  genObjectName_ = VertexShader <$> glCreateShader GL_VERTEX_SHADER

newtype FragmentShader = FragmentShader { _fragmentShaderGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)
instance ObjectName FragmentShader where
  isObjectName (FragmentShader n) = unmarshallGLboolean <$> glIsShader n
  deleteObjectName (FragmentShader n) = glDeleteShader n
instance GeneratableObjectName FragmentShader where
  genObjectName_ = FragmentShader <$> glCreateShader GL_FRAGMENT_SHADER

newtype TessControlShader = TessControlShader { _tessControlShaderGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)
instance ObjectName TessControlShader where
  isObjectName (TessControlShader n) = unmarshallGLboolean <$> glIsShader n
  deleteObjectName (TessControlShader n) = glDeleteShader n
instance GeneratableObjectName TessControlShader where
  genObjectName_ = TessControlShader <$> glCreateShader GL_TESS_CONTROL_SHADER

newtype TessEvalShader = TessEvalShader { _tessEvalShaderGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)
instance ObjectName TessEvalShader where
  isObjectName (TessEvalShader n) = unmarshallGLboolean <$> glIsShader n
  deleteObjectName (TessEvalShader n) = glDeleteShader n
instance GeneratableObjectName TessEvalShader where
  genObjectName_ = TessEvalShader <$> glCreateShader GL_TESS_EVALUATION_SHADER

newtype ComputeShader = ComputeShader { _computeShaderGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)
instance ObjectName ComputeShader where
  isObjectName (ComputeShader n) = unmarshallGLboolean <$> glIsShader n
  deleteObjectName (ComputeShader n) = glDeleteShader n
instance GeneratableObjectName ComputeShader where
  genObjectName_ = ComputeShader <$> glCreateShader GL_COMPUTE_SHADER

class GeneratableObjectName t => Shader t where
  marshallShaderType :: t -> GLuint
  marshallShaderObject :: t -> GLuint

instance Shader VertexShader where
  marshallShaderType _ = GL_VERTEX_SHADER
  marshallShaderObject = _vertexShaderGLuint
instance Shader TessEvalShader where
  marshallShaderType _ = GL_TESS_EVALUATION_SHADER
  marshallShaderObject = _tessEvalShaderGLuint
instance Shader TessControlShader where
  marshallShaderType _ = GL_TESS_CONTROL_SHADER
  marshallShaderObject = _tessControlShaderGLuint
instance Shader FragmentShader where
  marshallShaderType _ = GL_FRAGMENT_SHADER
  marshallShaderObject = _fragmentShaderGLuint
instance Shader ComputeShader where
  marshallShaderType _ = GL_COMPUTE_SHADER
  marshallShaderObject = _computeShaderGLuint

compileShader :: (Shader t, MonadIO m) => t -> m (Maybe ByteString)
compileShader t = do
  glCompileShader n
  status <- unmarshallGLboolean <$> foreignPoke (glGetShaderiv n GL_COMPILE_STATUS)
  if status
    then return Nothing
    else Just <$> withForeignBufferBS (glGetShaderiv n GL_INFO_LOG_LENGTH) (glGetShaderInfoLog n)
  where
    n = marshallShaderObject t

shaderDeleteStatus :: (Shader t, MonadIO m) => t -> m Bool
shaderDeleteStatus shader = unmarshallGLboolean <$> foreignPoke (glGetShaderiv (marshallShaderObject shader) GL_DELETE_STATUS)

shaderInfoLog :: (Shader t, MonadIO m) => t -> m ByteString
shaderInfoLog shader = withForeignBufferBS (glGetShaderiv n GL_INFO_LOG_LENGTH) (glGetShaderInfoLog n)
  where
    n = marshallShaderObject shader

shaderSource :: Shader t => t -> StateVar ByteString
shaderSource shader = makeStateVar getSource setSource
    where
      n = marshallShaderObject shader
      getSource = withForeignBufferBS (glGetShaderiv n GL_SHADER_SOURCE_LENGTH) (glGetShaderSource n)
      setSource src =
        withByteString src $ \srcPtr srcLength ->
        with srcPtr $ \srcPtrBuf ->
                        with srcLength $ \srcLengthBuf ->
                                           glShaderSource n 1 srcPtrBuf srcLengthBuf
