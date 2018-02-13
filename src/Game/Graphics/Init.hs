{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Game.Graphics.Init where

import ClassyPrelude
import Game.Types
import qualified Graphics.UI.GLFW as G

mioWindowHint :: MonadIO m => G.WindowHint -> m ()
mioWindowHint = liftIO . G.windowHint

createGraphicsContext :: MonadIO m => GraphicsContext -> m ()
createGraphicsContext GraphicsContext {..} = do
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

createWindow :: MonadIO m => WindowConfig -> m G.Window
createWindow WindowConfig {..} = do
  Just win <- liftIO $ G.createWindow _windowConfigWidth _windowConfigHeight _windowConfigTitle _windowConfigMonitorFullscreen _windowConfigWindowContextShare
  mapM_ mioWindowHint
    [ G.WindowHint'Resizable _windowConfigResizable
    , G.WindowHint'Visible _windowConfigVisible
    , G.WindowHint'Decorated _windowConfigDecorated
    ]
  return win
