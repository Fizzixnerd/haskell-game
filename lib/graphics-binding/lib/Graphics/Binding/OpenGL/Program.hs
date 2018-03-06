{-# LANGUAGE LambdaCase #-}

module Graphics.Binding.OpenGL.Program where

import Graphics.GL.Core45
import Data.StateVar
import Graphics.Binding.OpenGL.Utils
import Graphics.Binding.OpenGL.Types
import Data.ByteString

programDeleteStatus :: MonadIO m => Program -> m Bool
programDeleteStatus (Program n) = unmarshalGLboolean <$> foreignPoke (glGetProgramiv n GL_DELETE_STATUS)

attachShader :: (MonadIO m, Shader t) => Program -> t -> m ()
attachShader (Program n) t = glAttachShader n (marshalShaderObject t)

linkProgram :: MonadIO m => Program -> m (Maybe ByteString)
linkProgram (Program n) = liftIO $ do
  glLinkProgram n
  status <- unmarshalGLboolean <$> foreignPoke (glGetProgramiv n GL_LINK_STATUS)
  if status
    then return Nothing
    else Just <$> withForeignBufferBS (glGetProgramiv n GL_INFO_LOG_LENGTH) (glGetProgramInfoLog n)

validateProgram :: MonadIO m => Program -> m (Maybe ByteString)
validateProgram (Program n) = liftIO $ do
  glValidateProgram n
  status <- unmarshalGLboolean <$> foreignPoke (glGetProgramiv n GL_VALIDATE_STATUS)
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
      Nothing -> glUseProgram 0
      Just (Program n) -> glUseProgram n

useProgram :: MonadIO m => Program -> m ()
useProgram (Program n) = glUseProgram n
