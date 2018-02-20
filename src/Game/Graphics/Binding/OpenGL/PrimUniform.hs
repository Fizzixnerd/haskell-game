{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Graphics.Binding.OpenGL.PrimUniform where

import qualified Data.Vector.Storable as VS
import           Game.Graphics.Binding.OpenGL.Shader
import           Game.Graphics.Binding.OpenGL.Program
import           Game.Graphics.Binding.OpenGL.Utils
import           Graphics.GL.Core45
import           Graphics.GL.Types
import           Linear
-- NB everything is transposed by default. Will fix.

newtype UniformLocation = UniformLocation
  { uniformLocationInternal :: GLint
  } deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

class Storable a => PrimUniform a where
  primMarshall :: MonadIO m => Program -> UniformLocation -> a -> m ()
  primMarshallArray :: MonadIO m => Program -> UniformLocation -> VS.Vector a -> m ()

instance PrimUniform GLint where
  primMarshall (Program n) (UniformLocation m) = glProgramUniform1i n m
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ glProgramUniform1iv n m
instance PrimUniform GLuint where
  primMarshall (Program n) (UniformLocation m) = glProgramUniform1ui n m
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ glProgramUniform1uiv n m
instance PrimUniform GLfloat where
  primMarshall (Program n) (UniformLocation m) = glProgramUniform1f n m
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ glProgramUniform1fv n m
instance PrimUniform GLdouble where
  primMarshall (Program n) (UniformLocation m) = glProgramUniform1d n m
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ glProgramUniform1dv n m

instance PrimUniform (V1 GLint) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform1iv n m 1 . castPtr
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform1iv n m len (castPtr ptr)
instance PrimUniform (V1 GLuint) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform1uiv n m 1 . castPtr
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform1uiv n m len (castPtr ptr)
instance PrimUniform (V1 GLfloat) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform1fv n m 1 . castPtr
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform1fv n m len (castPtr ptr)
instance PrimUniform (V1 GLdouble) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform1dv n m 1 . castPtr
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform1dv n m len (castPtr ptr)

instance PrimUniform (V2 GLint) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform2iv n m 1 . castPtr
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform2iv n m len (castPtr ptr)
instance PrimUniform (V2 GLuint) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform2uiv n m 1 . castPtr
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform2uiv n m len (castPtr ptr)
instance PrimUniform (V2 GLfloat) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform2fv n m 1 . castPtr
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform2fv n m len (castPtr ptr)
instance PrimUniform (V2 GLdouble) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform2dv n m 1 . castPtr
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform2dv n m len (castPtr ptr)

instance PrimUniform (V3 GLint) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform3iv n m 1 . castPtr
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform3iv n m len (castPtr ptr)
instance PrimUniform (V3 GLuint) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform3uiv n m 1 . castPtr
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform3uiv n m len (castPtr ptr)
instance PrimUniform (V3 GLfloat) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform3fv n m 1 . castPtr
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform3fv n m len (castPtr ptr)
instance PrimUniform (V3 GLdouble) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform3dv n m 1 . castPtr
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform3dv n m len (castPtr ptr)

instance PrimUniform (V4 GLint) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform4iv n m 1 . castPtr
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform4iv n m len (castPtr ptr)
instance PrimUniform (V4 GLuint) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform4uiv n m 1 . castPtr
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform4uiv n m len (castPtr ptr)
instance PrimUniform (V4 GLfloat) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform4fv n m 1 . castPtr
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform4fv n m len (castPtr ptr)
instance PrimUniform (V4 GLdouble) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform4dv n m 1 . castPtr
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform4dv n m len (castPtr ptr)

-- Column major!
instance PrimUniform (M22 GLfloat) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix2fv n m 1 GL_TRUE (castPtr ptr)
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix2fv n m len GL_TRUE (castPtr ptr)
instance PrimUniform (M23 GLfloat) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix2x3fv n m 1 GL_TRUE (castPtr ptr)
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix2x3fv n m len GL_TRUE (castPtr ptr)
instance PrimUniform (M24 GLfloat) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix2x4fv n m 1 GL_TRUE (castPtr ptr)
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix2x4fv n m len GL_TRUE (castPtr ptr)
instance PrimUniform (M32 GLfloat) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix3x2fv n m 1 GL_TRUE (castPtr ptr)
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix3x2fv n m len GL_TRUE (castPtr ptr)
instance PrimUniform (M33 GLfloat) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix3fv n m 1 GL_TRUE (castPtr ptr)
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix3fv n m len GL_TRUE (castPtr ptr)
instance PrimUniform (M34 GLfloat) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix3x4fv n m 1 GL_TRUE (castPtr ptr)
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix3x4fv n m len GL_TRUE (castPtr ptr)
instance PrimUniform (M42 GLfloat) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix4x2fv n m 1 GL_TRUE (castPtr ptr)
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix4x2fv n m len GL_TRUE (castPtr ptr)
instance PrimUniform (M43 GLfloat) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix4x3fv n m 1 GL_TRUE (castPtr ptr)
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix4x3fv n m len GL_TRUE (castPtr ptr)
instance PrimUniform (M44 GLfloat) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix4fv n m 1 GL_TRUE (castPtr ptr)
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix4fv n m len GL_TRUE (castPtr ptr)

instance PrimUniform (M22 GLdouble) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix2dv n m 1 GL_TRUE (castPtr ptr)
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix2dv n m len GL_TRUE (castPtr ptr)
instance PrimUniform (M23 GLdouble) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix2x3dv n m 1 GL_TRUE (castPtr ptr)
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix2x3dv n m len GL_TRUE (castPtr ptr)
instance PrimUniform (M24 GLdouble) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix2x4dv n m 1 GL_TRUE (castPtr ptr)
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix2x4dv n m len GL_TRUE (castPtr ptr)
instance PrimUniform (M32 GLdouble) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix3x2dv n m 1 GL_TRUE (castPtr ptr)
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix3x2dv n m len GL_TRUE (castPtr ptr)
instance PrimUniform (M33 GLdouble) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix3dv n m 1 GL_TRUE (castPtr ptr)
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix3dv n m len GL_TRUE (castPtr ptr)
instance PrimUniform (M34 GLdouble) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix3x4dv n m 1 GL_TRUE (castPtr ptr)
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix3x4dv n m len GL_TRUE (castPtr ptr)
instance PrimUniform (M42 GLdouble) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix4x2dv n m 1 GL_TRUE (castPtr ptr)
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix4x2dv n m len GL_TRUE (castPtr ptr)
instance PrimUniform (M43 GLdouble) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix4x3dv n m 1 GL_TRUE (castPtr ptr)
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix4x3dv n m len GL_TRUE (castPtr ptr)
instance PrimUniform (M44 GLdouble) where
  primMarshall (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix4dv n m 1 GL_TRUE (castPtr ptr)
  primMarshallArray (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix4dv n m len GL_TRUE (castPtr ptr)
