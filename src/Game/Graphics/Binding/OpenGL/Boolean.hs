module Game.Graphics.Binding.OpenGL.Boolean where

import Graphics.GL.Types
import Graphics.GL.Core45

marshallGLboolean :: Bool -> GLboolean
marshallGLboolean x = if x then GL_TRUE else GL_FALSE

unmarshallGLboolean :: (Eq a, Num a) => a -> Bool
unmarshallGLboolean = (/= GL_FALSE)
