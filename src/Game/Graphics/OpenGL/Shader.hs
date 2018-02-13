{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Game.Graphics.OpenGL.Shader where

import Game.Graphics.OpenGL.ObjectName
import Game.Graphics.OpenGL.Utils
import Game.Graphics.OpenGL.Boolean
import Graphics.GL.Types
import Graphics.GL.Core45
import Data.ByteString
import Data.StateVar

newtype VertexShader = VertexShader { _vertexShaderGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)
instance ObjectName VertexShader where
  isObjectName (VertexShader n) = unmarshallGLboolean <$> glIsShader n
  deleteObjectName (VertexShader n) = glDeleteShader n
instance GeneratableObjectName VertexShader where
  genObjectName = VertexShader <$> glCreateShader GL_VERTEX_SHADER

newtype FragmentShader = FragmentShader { _fragmentShaderGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)
instance ObjectName FragmentShader where
  isObjectName (FragmentShader n) = unmarshallGLboolean <$> glIsShader n
  deleteObjectName (FragmentShader n) = glDeleteShader n
instance GeneratableObjectName FragmentShader where
  genObjectName = FragmentShader <$> glCreateShader GL_FRAGMENT_SHADER

newtype TessControlShader = TessControlShader { _tessControlShaderGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)
instance ObjectName TessControlShader where
  isObjectName (TessControlShader n) = unmarshallGLboolean <$> glIsShader n
  deleteObjectName (TessControlShader n) = glDeleteShader n
instance GeneratableObjectName TessControlShader where
  genObjectName = TessControlShader <$> glCreateShader GL_TESS_CONTROL_SHADER

newtype TessEvalShader = TessEvalShader { _tessEvalShaderGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)
instance ObjectName TessEvalShader where
  isObjectName (TessEvalShader n) = unmarshallGLboolean <$> glIsShader n
  deleteObjectName (TessEvalShader n) = glDeleteShader n
instance GeneratableObjectName TessEvalShader where
  genObjectName = TessEvalShader <$> glCreateShader GL_TESS_EVALUATION_SHADER

newtype ComputeShader = ComputeShader { _computeShaderGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)
instance ObjectName ComputeShader where
  isObjectName (ComputeShader n) = unmarshallGLboolean <$> glIsShader n
  deleteObjectName (ComputeShader n) = glDeleteShader n
instance GeneratableObjectName ComputeShader where
  genObjectName = ComputeShader <$> glCreateShader GL_COMPUTE_SHADER

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

newtype Program = Program { getProgramGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)
instance ObjectName Program where
  isObjectName (Program n) = unmarshallGLboolean <$> glIsProgram n
  deleteObjectName (Program n) = glDeleteProgram n

instance GeneratableObjectName Program where
  genObjectName = Program <$> glCreateProgram

programDeleteStatus :: MonadIO m => Program -> m Bool
programDeleteStatus (Program n) = unmarshallGLboolean <$> foreignPoke (glGetProgramiv n GL_DELETE_STATUS)

attachShader :: (MonadIO m, Shader t) => Program -> t -> m ()
attachShader (Program n) t = glAttachShader n (marshallShaderObject t)

linkProgram :: MonadIO m => Program -> m (Maybe ByteString)
linkProgram (Program n) = liftIO $ do
  glLinkProgram n
  status <- unmarshallGLboolean <$> foreignPoke (glGetProgramiv n GL_LINK_STATUS)
  if status
    then return Nothing
    else Just <$> withForeignBufferBS (glGetProgramiv n GL_INFO_LOG_LENGTH) (glGetProgramInfoLog n)

validateProgram :: MonadIO m => Program -> m (Maybe ByteString)
validateProgram (Program n) = liftIO $ do
  glValidateProgram n
  status <- unmarshallGLboolean <$> foreignPoke (glGetProgramiv n GL_VALIDATE_STATUS)
  if status
    then return Nothing
    else Just <$> withForeignBufferBS (glGetProgramiv n GL_INFO_LOG_LENGTH) (glGetProgramInfoLog n)

currentProgram :: StateVar (Maybe Program)
currentProgram = makeStateVar getP setP
  where
    getP = do
      n <- foreignPoke (glGetInteger64v GL_CURRENT_PROGRAM)
      if n == 0
        then return Nothing
        else return . Just . Program . fromIntegral $ n
    setP = \case
      Nothing -> return ()
      Just (Program n) -> glUseProgram n
