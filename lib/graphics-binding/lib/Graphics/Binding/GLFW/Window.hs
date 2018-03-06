{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.Binding.GLFW.Window
  ( module X
  , contextVersion
  , GLContextVersion(..)
  , defaultGraphicsContext
  , WindowConfig(..)
  , defaultWindowConfig
  , windowShouldClose
  , windowCloseCallback
  , withWindow
  , withGraphicsContext
  , cursorInputMode
  , graphicsContextClientAPI
  , graphicsContextContextVersionMajor
  , graphicsContextContextVersionMinor
  , graphicsContextContextRobustness
  , graphicsContextOpenGLForwardCompat
  , graphicsContextOpenGLDebugContext
  , graphicsContextOpenGLProfile
  , graphicsContextRefreshRate
  , graphicsContextRedBits
  , graphicsContextGreenBits
  , graphicsContextBlueBits
  , graphicsContextAlphaBits
  , graphicsContextDepthBits
  , graphicsContextStencilBits
  , graphicsContextSamples
  , graphicsContextStereo
  , graphicsContextSRGBCapable
  , windowConfigResizable
  , windowConfigVisible
  , windowConfigDecorated
  , windowConfigWidth
  , windowConfigHeight
  , windowConfigTitle
  , windowConfigMonitorFullscreen
  , windowConfigWindowContextShare
  , contextCurrent
  ) where

import Graphics.UI.GLFW as X
  ( Window
  , OpenGLProfile(..)
  , ContextRobustness(..)
  , WindowCloseCallback
  , CursorInputMode(..)
  , WindowHint(..)
  , swapBuffers
  , MouseButton(..)
  , Key(..)
  )

import qualified Graphics.UI.GLFW             as G
import           Data.StateVar
import           Control.Monad.IO.Class
import           ClassyPrelude
import           Control.Lens

data GLContextVersion = GLContextVersion
  { _versionMajor  :: Int
  , _versionMinor  :: Int
  , _versionRevis  :: Int
  } deriving (Eq, Ord, Show)

contextVersion :: G.Window -> GettableStateVar GLContextVersion
contextVersion win = do
  major <- G.getWindowContextVersionMajor win
  minor <- G.getWindowContextVersionMinor win
  revis <- G.getWindowContextVersionRevision win
  return $ GLContextVersion major minor revis

data GraphicsContextConfig = GraphicsContextConfig
  { _graphicsContextClientAPI           :: G.ClientAPI
  , _graphicsContextContextVersionMajor :: Int
  , _graphicsContextContextVersionMinor :: Int
  , _graphicsContextContextRobustness   :: G.ContextRobustness
  , _graphicsContextOpenGLForwardCompat :: Bool
  , _graphicsContextOpenGLDebugContext  :: Bool
  , _graphicsContextOpenGLProfile       :: G.OpenGLProfile
  , _graphicsContextRefreshRate         :: Maybe Int
  , _graphicsContextRedBits             :: Int
  , _graphicsContextGreenBits           :: Int
  , _graphicsContextBlueBits            :: Int
  , _graphicsContextAlphaBits           :: Int
  , _graphicsContextDepthBits           :: Int
  , _graphicsContextStencilBits         :: Int
  , _graphicsContextSamples             :: Int
  , _graphicsContextStereo              :: Bool
  , _graphicsContextSRGBCapable         :: Bool
  } deriving (Eq, Ord, Show)

defaultGraphicsContext :: GraphicsContextConfig
defaultGraphicsContext = GraphicsContextConfig
  { _graphicsContextClientAPI           = G.ClientAPI'OpenGL
  , _graphicsContextContextVersionMajor = 4
  , _graphicsContextContextVersionMinor = 5
  , _graphicsContextContextRobustness   = G.ContextRobustness'NoRobustness
  , _graphicsContextOpenGLForwardCompat = True
  , _graphicsContextOpenGLDebugContext  = True
  , _graphicsContextOpenGLProfile       = G.OpenGLProfile'Core
  , _graphicsContextRefreshRate         = Nothing
  , _graphicsContextRedBits             = 8
  , _graphicsContextGreenBits           = 8
  , _graphicsContextBlueBits            = 8
  , _graphicsContextAlphaBits           = 8
  , _graphicsContextDepthBits           = 24
  , _graphicsContextStencilBits         = 8
  , _graphicsContextSamples             = 4
  , _graphicsContextStereo              = False
  , _graphicsContextSRGBCapable         = False
  }

data WindowConfig = WindowConfig
  { _windowConfigResizable          :: Bool
  , _windowConfigVisible            :: Bool
  , _windowConfigDecorated          :: Bool
  , _windowConfigWidth              :: Int
  , _windowConfigHeight             :: Int
  , _windowConfigTitle              :: String
  , _windowConfigMonitorFullscreen  :: Maybe G.Monitor
  , _windowConfigWindowContextShare :: Maybe G.Window
  } deriving (Eq, Ord, Show)

defaultWindowConfig :: WindowConfig
defaultWindowConfig = WindowConfig
  { _windowConfigResizable          = True
  , _windowConfigVisible            = True
  , _windowConfigDecorated          = True
  , _windowConfigWidth              = 1920
  , _windowConfigHeight             = 1080
  , _windowConfigTitle              = ""
  , _windowConfigMonitorFullscreen  = Nothing
  , _windowConfigWindowContextShare = Nothing
  }

windowShouldClose :: Window -> StateVar Bool
windowShouldClose win = makeStateVar get_ set_
  where
    get_ = G.windowShouldClose win
    set_ = G.setWindowShouldClose win

windowCloseCallback :: Window -> SettableStateVar (Maybe WindowCloseCallback)
windowCloseCallback win = makeSettableStateVar $ G.setWindowCloseCallback win

cursorInputMode :: Window -> StateVar CursorInputMode
cursorInputMode win = makeStateVar get_ set_
  where
    get_ = G.getCursorInputMode win
    set_ = G.setCursorInputMode win

withWindow :: forall m b. (MonadIO m, MonadMask m) => WindowConfig -> (Window -> m b) -> m b
withWindow WindowConfig {..} = bracket init_ destroy_
  where
    mioWindowHint = liftIO . G.windowHint
    init_ = do
      Just win <- liftIO $ G.createWindow _windowConfigWidth _windowConfigHeight _windowConfigTitle _windowConfigMonitorFullscreen _windowConfigWindowContextShare
      mapM_ mioWindowHint
        [ G.WindowHint'Resizable _windowConfigResizable
        , G.WindowHint'Visible _windowConfigVisible
        , G.WindowHint'Decorated _windowConfigDecorated
        ]
      return win
    destroy_ win = liftIO $ G.destroyWindow win

withGraphicsContext :: forall m b. (MonadIO m , MonadMask m) => GraphicsContextConfig -> m b -> m b
withGraphicsContext GraphicsContextConfig {..} = bracket_ init_ destroy_
  where
    mioWindowHint = liftIO . G.windowHint
    init_ = do
      _ <- liftIO G.init
      mapM_ mioWindowHint
        [ G.WindowHint'ClientAPI _graphicsContextClientAPI
        , G.WindowHint'ContextVersionMajor _graphicsContextContextVersionMajor
        , G.WindowHint'ContextVersionMinor _graphicsContextContextVersionMinor
        , G.WindowHint'ContextRobustness _graphicsContextContextRobustness
        , G.WindowHint'OpenGLForwardCompat _graphicsContextOpenGLForwardCompat
        , G.WindowHint'OpenGLDebugContext _graphicsContextOpenGLDebugContext
        , G.WindowHint'OpenGLProfile _graphicsContextOpenGLProfile
        , G.WindowHint'RedBits _graphicsContextRedBits
        , G.WindowHint'GreenBits _graphicsContextGreenBits
        , G.WindowHint'BlueBits _graphicsContextBlueBits
        , G.WindowHint'AlphaBits _graphicsContextAlphaBits
        , G.WindowHint'DepthBits _graphicsContextDepthBits
        , G.WindowHint'StencilBits _graphicsContextStencilBits
        , G.WindowHint'Samples _graphicsContextSamples
        , G.WindowHint'Stereo _graphicsContextStereo
        , G.WindowHint'sRGBCapable _graphicsContextSRGBCapable
        ]
      forM_ _graphicsContextRefreshRate $ mioWindowHint . G.WindowHint'RefreshRate
    destroy_ = liftIO G.terminate

contextCurrent :: StateVar (Maybe Window)
contextCurrent = makeStateVar get_ set_
  where
    get_ = G.getCurrentContext
    set_ = G.makeContextCurrent

mconcat <$> mapM makeLenses
  [ ''WindowConfig
  , ''GraphicsContextConfig
  ]
