{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Graphics.Binding.OpenGL.VertexArray where

import Data.StateVar
import Data.Vector.Storable as VS
import Game.Graphics.Binding.OpenGL.Boolean
import Game.Graphics.Binding.OpenGL.BufferObject
import Game.Graphics.Binding.OpenGL.DataType
import Game.Graphics.Binding.OpenGL.ObjectName
import Game.Graphics.Binding.OpenGL.Utils
import Graphics.GL.Types
import Graphics.GL.Core45

newtype VertexArrayObject = VertexArrayObject
  { getVertexArrayObjectGLuint :: GLuint
  } deriving (Eq, Ord, Show, Storable)

instance ObjectName VertexArrayObject where
  isObjectName (VertexArrayObject n) = unmarshallGLboolean <$> glIsVertexArray n
  deleteObjectNames ns = liftIO . VS.unsafeWith ns $ \ptr -> glDeleteVertexArrays len (castPtr ptr)
    where
      len = fromIntegral $ VS.length ns

instance GeneratableObjectName VertexArrayObject where
  genObjectNames_ n = VS.map VertexArrayObject <$> withForeignBufferVec n (glCreateVertexArrays(fromIntegral n))

newtype AttribLocation = AttribLocation { getAttribLocationGLuint :: GLuint } deriving (Eq, Ord, Show)

--------------------------------------------
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
  = glVertexArrayAttribFormat vaobj attribindx (fromIntegral size) (marshallGLDataType typ) handleFlag (fromIntegral relOffset)
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
