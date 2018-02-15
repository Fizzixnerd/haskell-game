{-# LANGUAGE LambdaCase #-}

module Game.Graphics.OpenGL.Window where

import Graphics.GL.Types
import Graphics.GL.Core45
import Data.StateVar
import Linear

-- Some things in here, like the definition of Face for cullFace
-- probably shouldn't be in here. Will have to move if things get
-- more complex.

data Face = Front | Back | FrontBack deriving (Eq, Ord, Show)

cullFace :: SettableStateVar (Maybe Face)
cullFace = makeSettableStateVar $
  \case
    Nothing        -> glDisable GL_CULL_FACE
    Just Front     -> glEnable GL_CULL_FACE >> glCullFace GL_FRONT
    Just Back      -> glEnable GL_CULL_FACE >> glCullFace GL_BACK
    Just FrontBack -> glEnable GL_CULL_FACE >> glCullFace GL_FRONT_AND_BACK

newtype Color4 = Color4 (V4 GLfloat)
color4 :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Color4
color4 r g b = Color4 . V4 r g b

clearColor :: SettableStateVar Color4
clearColor = makeSettableStateVar $ \(Color4 (V4 r g b a))
  -> glClearColor r g b a

data DepthFunc
  = DepthNever
  | DepthLess
  | DepthEqual
  | DepthLEqual
  | DepthGreater
  | DepthNotEqual
  | DepthGEqual
  | DepthAlways
  deriving (Eq, Ord, Show)

depthFunc :: SettableStateVar (Maybe DepthFunc)
depthFunc = makeSettableStateVar $ \case
  Nothing -> glDisable GL_DEPTH_TEST
  Just DepthNever     -> glEnable GL_DEPTH_TEST >> glDepthFunc GL_NEVER
  Just DepthLess      -> glEnable GL_DEPTH_TEST >> glDepthFunc GL_LESS
  Just DepthEqual     -> glEnable GL_DEPTH_TEST >> glDepthFunc GL_EQUAL
  Just DepthLEqual    -> glEnable GL_DEPTH_TEST >> glDepthFunc GL_LEQUAL
  Just DepthGreater   -> glEnable GL_DEPTH_TEST >> glDepthFunc GL_GREATER
  Just DepthNotEqual  -> glEnable GL_DEPTH_TEST >> glDepthFunc GL_NOTEQUAL
  Just DepthGEqual    -> glEnable GL_DEPTH_TEST >> glDepthFunc GL_GEQUAL
  Just DepthAlways    -> glEnable GL_DEPTH_TEST >> glDepthFunc GL_ALWAYS
