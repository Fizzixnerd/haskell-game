module Graphics.Binding.OpenGL.Boolean where

import Graphics.GL.Types
import Graphics.GL.Core45

marshalGLboolean :: Bool -> GLboolean
marshalGLboolean x = if x then GL_TRUE else GL_FALSE

unmarshalGLboolean :: (Eq a, Num a) => a -> Bool
unmarshalGLboolean = (/= GL_FALSE)
