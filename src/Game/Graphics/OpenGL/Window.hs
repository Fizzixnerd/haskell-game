{-# LANGUAGE LambdaCase #-}

module Game.Graphics.OpenGL.Window where

import Graphics.GL.Types
import Graphics.GL.Core45
import Data.StateVar
import Linear
import Data.ByteString
import Foreign.Ptr

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

data DebugSource
  = DebugSourceAPI
  | DebugSourceWindowSystem
  | DebugSourceShaderCompiler
  | DebugSourceThirdParty
  | DebugSourceApplication
  | DebugSourceOther
  deriving (Eq, Ord, Show)

data DebugType
  = DebugTypeError
  | DebugTypeDeprecatedBehavior
  | DebugTypeUndefinedBehavior
  | DebugTypePortability
  | DebugTypePerformance
  | DebugTypeMarker
  | DebugTypePushGroup
  | DebugTypePopGroup
  | DebugTypeOther
  deriving (Eq, Ord, Show)

data DebugSeverity
  = DebugSeverityHigh
  | DebugSeverityMedium
  | DebugSeverityLow
  | DebugSeverityNotification
  deriving (Eq, Ord, Show)

newtype DebugID = DebugID GLuint deriving (Eq, Ord, Show)

type DebugCallbackFun = DebugSource -> DebugType -> DebugID -> DebugSeverity -> ByteString -> IO ()

unmarshallDebugSource :: GLenum -> DebugSource
unmarshallDebugSource = \case
  GL_DEBUG_SOURCE_API             -> DebugSourceAPI
  GL_DEBUG_SOURCE_WINDOW_SYSTEM   -> DebugSourceWindowSystem
  GL_DEBUG_SOURCE_SHADER_COMPILER -> DebugSourceShaderCompiler
  GL_DEBUG_SOURCE_THIRD_PARTY     -> DebugSourceThirdParty
  GL_DEBUG_SOURCE_APPLICATION     -> DebugSourceApplication
  GL_DEBUG_SOURCE_OTHER           -> DebugSourceOther
  _                            -> error "Unrecognized debug source.\n"

unmarshallDebugType   :: GLenum -> DebugType
unmarshallDebugType = \case
  GL_DEBUG_TYPE_ERROR               -> DebugTypeError
  GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR -> DebugTypeDeprecatedBehavior
  GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR  -> DebugTypeUndefinedBehavior
  GL_DEBUG_TYPE_PORTABILITY         -> DebugTypePortability
  GL_DEBUG_TYPE_PERFORMANCE         -> DebugTypePerformance
  GL_DEBUG_TYPE_MARKER              -> DebugTypeMarker
  GL_DEBUG_TYPE_PUSH_GROUP          -> DebugTypePushGroup
  GL_DEBUG_TYPE_POP_GROUP           -> DebugTypePopGroup
  GL_DEBUG_TYPE_OTHER               -> DebugTypeOther
  _                              -> error "Unrecognized debug type.\n"

unmarshallDebugSeverity :: GLenum -> DebugSeverity
unmarshallDebugSeverity = \case
  GL_DEBUG_SEVERITY_HIGH         -> DebugSeverityHigh
  GL_DEBUG_SEVERITY_MEDIUM       -> DebugSeverityMedium
  GL_DEBUG_SEVERITY_LOW          -> DebugSeverityLow
  GL_DEBUG_SEVERITY_NOTIFICATION -> DebugSeverityNotification
  _                           -> error "Unrecognized debug severity.\n"

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

