{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Lib
    ( someFunc
    ) where

import ClassyPrelude
import qualified Graphics.UI.GLFW as G
import qualified Graphics.Rendering.OpenGL.GL as G
import qualified Graphics.Rendering.OpenGL.GL.Shaders as G
import qualified Graphics.Rendering.OpenGL.GL.DebugOutput as G
import qualified Data.ObjectName as G
import Reactive.Banana.Combinators as B
import Reactive.Banana.Frameworks as B
import Control.Event.Handler as B
import Foreign.C.Types
import Foreign
import System.Exit
import Text.Printf

graphicsInit :: IO ()
graphicsInit = do
  _ <- G.init
  G.windowHint $ G.WindowHint'ContextVersionMajor 4
  G.windowHint $ G.WindowHint'ContextVersionMinor 5
  G.windowHint $ G.WindowHint'OpenGLForwardCompat True
  G.windowHint $ G.WindowHint'Samples 4
  G.windowHint $ G.WindowHint'OpenGLProfile G.OpenGLProfile'Core
  G.windowHint $ G.WindowHint'OpenGLDebugContext True

withWindow :: (G.Window -> IO ()) -> IO ()
withWindow f = do
  mwin <- G.createWindow 800 600 "Haskell Game Hello World" Nothing Nothing
  case mwin of
    Nothing -> error "Could not create window."
    Just win -> f win

printContextVersion :: G.Window -> IO ()
printContextVersion win = do
  maj <- G.getWindowContextVersionMajor win
  min <- G.getWindowContextVersionMinor win
  rev <- G.getWindowContextVersionRevision win
  printf "%i.%i.%i\n" maj min rev

makeShader :: FilePath -> G.ShaderType -> IO G.Shader
makeShader shaderName shaderType = do
  shaderText <- readFile shaderName
  shader <- G.createShader shaderType
  G.shaderSourceBS shader G.$= shaderText
  G.compileShader shader
  G.shaderInfoLog shader >>= (\x -> if null x then return () else printf "%s\n\n" x)
  return shader

compileShaders :: IO G.Program
compileShaders = do
  vertexShader <- makeShader "shader.vs" G.VertexShader
  tessellationControlShader <- makeShader "shader.tcs" G.TessControlShader
  tessellationEvaluationShader <- makeShader "shader.tes" G.TessEvaluationShader
  geometryShader <- makeShader "shader.gs" G.GeometryShader
  fragmentShader <- makeShader "shader.fs" G.FragmentShader

  program <- G.createProgram
  G.attachShader program vertexShader
  G.attachShader program tessellationControlShader
  G.attachShader program tessellationEvaluationShader
  G.attachShader program geometryShader
  G.attachShader program fragmentShader
  G.linkProgram program
  G.validateProgram program
  G.programInfoLog program >>= (\x -> if null x then return () else printf "%s\n\n" x)

  G.deleteObjectNames [ fragmentShader
                      , tessellationControlShader
                      , tessellationEvaluationShader
                      , geometryShader
                      , vertexShader ]

  return program

makeArrayBuffer :: IO ()
makeArrayBuffer = do
  buf :: G.BufferObject <- G.genObjectName
  G.bindBuffer G.ArrayBuffer G.$= Just buf
  ptr <- mallocBytes 8
  poke ptr (69420 :: Int)
  G.bufferData G.ArrayBuffer G.$= (CPtrdiff 8, ptr, G.StreamDraw)

render :: G.VertexArrayObject -> G.Program -> IO ()
render vao p = do
  G.clear [G.ColorBuffer]
  G.currentProgram G.$= Just p
  G.drawArrays G.Patches 0 3

someFunc :: IO ()
someFunc = do
  graphicsInit
  withWindow
    (\win -> do
        G.makeContextCurrent $ Just win

        -- get the Handlers we need.
        (addHandlerShouldClose, fireShouldClose) <- B.newAddHandler
        (addHandlerTick, fireTick) <- B.newAddHandler
        
        let tellWindowToClose = G.setWindowShouldClose win True
            network :: IO () = mdo
              return ()
              -- \w -> do
              --   render vertexArrayObject prog
              --   G.swapBuffers w)
        
        G.debugMessageCallback G.$= Just (printf "!!!%s!!!\n\n" . show)
        printContextVersion win
        G.setWindowCloseCallback win (Just $ \w -> fireShouldClose w) 

        prog <- compileShaders
        G.polygonMode G.$= (G.Line, G.Line)
        G.pointSize G.$= 5.0
        G.clearColor G.$= G.Color4 0.5 0 0 0
        G.viewport G.$= (G.Position 0 0, G.Size 800 600)

        vertexArrayObject :: G.VertexArrayObject <- G.genObjectName
        makeArrayBuffer
        let offsetLocation = G.AttribLocation 0
            colorLocation  = G.AttribLocation 1
        G.vertexAttrib4 offsetLocation (0.5 :: Float) 0.5 0.0 0.0
        G.vertexAttrib4 colorLocation (0.8 :: Float) 1.0 0.0 1.0
        G.bindVertexArrayObject G.$= Just vertexArrayObject
        G.setWindowRefreshCallback win (Just $ (\w -> fireTick w))

        loop win vertexArrayObject prog
        G.deleteObjectName vertexArrayObject
        G.deleteObjectName prog
        G.destroyWindow win
        G.terminate
  where
    loop w vao p = do
      G.pollEvents
      sc <- G.windowShouldClose w
      unless sc $ loop w vao p
