{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Graphics.Binding.OpenGL.Program where

import Graphics.GL.Core45
import Graphics.GL.Types
import Game.Graphics.Binding.OpenGL.Shader
import Game.Graphics.Binding.OpenGL.ObjectName
import Data.StateVar
import Foreign.Storable
import Game.Graphics.Binding.OpenGL.Utils
import Game.Graphics.Binding.OpenGL.Boolean
import Data.ByteString

newtype Program = Program { getProgramGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)

instance ObjectName Program where
  isObjectName (Program n) = unmarshallGLboolean <$> glIsProgram n
  deleteObjectName (Program n) = glDeleteProgram n

instance GeneratableObjectName Program where
  genObjectName_ = Program <$> glCreateProgram

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

class GeneratableObjectName a => ProgramLike a where
  programDeleteStatus :: MonadIO m => a -> m Bool
  attachShader        :: (MonadIO m, Shader t) => a -> t -> m ()
  linkProgram         :: MonadIO m => a -> m (Maybe ByteString)
  validateProgram     :: MonadIO m => a -> m (Maybe ByteString)
  useProgram          :: MonadIO m => a -> m ()

instance ProgramLike Program where
  programDeleteStatus (Program n) = unmarshallGLboolean <$> foreignPoke (glGetProgramiv n GL_DELETE_STATUS)

  attachShader (Program n) t = glAttachShader n (marshallShaderObject t)

  linkProgram (Program n) = liftIO $ do
    glLinkProgram n
    status <- unmarshallGLboolean <$> foreignPoke (glGetProgramiv n GL_LINK_STATUS)
    if status
      then return Nothing
      else Just <$> withForeignBufferBS (glGetProgramiv n GL_INFO_LOG_LENGTH) (glGetProgramInfoLog n)

  validateProgram (Program n) = liftIO $ do
    glValidateProgram n
    status <- unmarshallGLboolean <$> foreignPoke (glGetProgramiv n GL_VALIDATE_STATUS)
    if status
      then return Nothing
      else Just <$> withForeignBufferBS (glGetProgramiv n GL_INFO_LOG_LENGTH) (glGetProgramInfoLog n)

  useProgram (Program n) = glUseProgram n

