{-# LANGUAGE LambdaCase #-}

module Graphics.Binding.OpenGL.Window where

import Data.ByteString
import Data.StateVar
import Foreign.Ptr
import Graphics.GL.Types
import Graphics.GL.Core45
import Graphics.Binding.OpenGL.Types
import Linear
import Text.Printf

-- Some things in here, like the definition of Face for cullFace
-- probably shouldn't be in here. Will have to move if things get
-- more complex.

cullFace :: SettableStateVar (Maybe Face)
cullFace = makeSettableStateVar $
  \case
    Nothing        -> glDisable GL_CULL_FACE
    Just Front     -> glEnable GL_CULL_FACE >> glCullFace GL_FRONT
    Just Back      -> glEnable GL_CULL_FACE >> glCullFace GL_BACK
    Just FrontBack -> glEnable GL_CULL_FACE >> glCullFace GL_FRONT_AND_BACK

color4 :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Color4
color4 r g b = Color4 . V4 r g b

clearColor :: SettableStateVar Color4
clearColor = makeSettableStateVar $ \(Color4 (V4 r g b a))
  -> glClearColor r g b a

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

debugMessageCallback :: SettableStateVar (Maybe DebugCallbackFun)
debugMessageCallback = makeSettableStateVar $ \case
  Nothing -> glDebugMessageCallback nullFunPtr nullPtr
  Just func -> do
    fp <- mkGLDEBUGPROC func'
    glDebugMessageCallback fp nullPtr
    where
      func' src typ ident sever len msg _ = do
        msg' <- packCStringLen (msg, fromIntegral len)
        func src' typ' id' sev' msg'
        where
          src' = unmarshallDebugSource src
          typ' = unmarshallDebugType typ
          id'  = DebugID ident
          sev' = unmarshallDebugSeverity sever

simpleDebugFunc :: DebugCallbackFun
simpleDebugFunc src typ ident sever msg = printf "!!! OpenGL Error. %s; %s; %s; %s;\n Message: %s\n\n" (show sever) (show ident) (show src) (show typ) (show msg)
