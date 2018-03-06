{-# LANGUAGE LambdaCase #-}

module Graphics.Binding.OpenGL.VertexArray where

import Data.StateVar
import Graphics.Binding.OpenGL.Types
import Graphics.Binding.OpenGL.Utils
import Graphics.GL.Core45

currentVertexArrayObject :: StateVar (Maybe VertexArrayObject)
currentVertexArrayObject = makeStateVar get' set'
  where
    get' = do
      n <- foreignPoke (glGetInteger64v GL_ARRAY_BUFFER_BINDING)
      if n == 0
        then return Nothing
        else return . Just . VertexArrayObject . fromIntegral $ n
    set' = \case
      Nothing -> return ()
      Just (VertexArrayObject n) -> glBindVertexArray n

vertexArrayAttribBinding :: MonadIO m => VertexArrayObject -> AttribLocation -> AttribLocation -> m ()
vertexArrayAttribBinding (VertexArrayObject n) (AttribLocation attribindex) (AttribLocation bindindex)
  = glVertexArrayAttribBinding n attribindex bindindex

vertexArrayVertexBuffer :: MonadIO m => VertexArrayObject -> AttribLocation -> BufferObject -> BufferObjectOffset -> BufferObjectStride -> m ()
vertexArrayVertexBuffer (VertexArrayObject n) (AttribLocation bindindx) (BufferObject bufobj) offset stride
  = glVertexArrayVertexBuffer n bindindx bufobj (fromIntegral offset) (fromIntegral stride)

vertexArrayAttribFormat :: MonadIO m => VertexArrayObject -> AttribLocation -> BufferObjectComponentSize -> GLDataType -> IntegerHandling -> BufferObjectRelOffset -> m ()
vertexArrayAttribFormat (VertexArrayObject vaobj) (AttribLocation attribindx) size typ integerHandling relOffset
  = glVertexArrayAttribFormat vaobj attribindx (fromIntegral size) (marshalGLDataType typ) handleFlag (fromIntegral relOffset)
  where
    handleFlag = case integerHandling of
      Normalized -> GL_TRUE
      NotNormalized -> GL_FALSE

vertexArrayAttribCapability :: MonadIO m => VertexArrayObject -> AttribLocation -> Capability -> m ()
vertexArrayAttribCapability (VertexArrayObject n) (AttribLocation loc) = \case
  Enabled  -> glEnableVertexArrayAttrib n loc
  Disabled -> glDisableVertexArrayAttrib n loc

bindElementBuffer :: MonadIO m => VertexArrayObject -> BufferObject -> m ()
bindElementBuffer (VertexArrayObject n) (BufferObject m) = glVertexArrayElementBuffer n m
