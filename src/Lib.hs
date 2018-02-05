{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( someFunc
    ) where

import ClassyPrelude
import qualified Graphics.UI.GLFW as G
import qualified Graphics.Rendering.OpenGL.GL as G
import qualified Graphics.Rendering.OpenGL.GL.Shaders as G
import qualified Data.ObjectName as G
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
  G.shaderInfoLog shader >>= print
  return shader

compileShaders :: IO (G.Program, G.VertexArrayObject)
compileShaders = do

  vertexShader <- makeShader "shader.vs" G.VertexShader
  tessellationControlShader <- makeShader "shader.tcs" G.TessControlShader
  tessellationEvaluationShader <- makeShader "shader.tes" G.TessEvaluationShader
  fragmentShader <- makeShader "shader.fs" G.FragmentShader

  program <- G.createProgram
  G.attachShader program vertexShader
  G.attachShader program tessellationControlShader
  G.attachShader program tessellationEvaluationShader
  G.attachShader program fragmentShader
  G.linkProgram program
  G.validateProgram program
  G.programInfoLog program >>= print
  vertexArrayObject :: G.VertexArrayObject <- G.genObjectName
  G.bindVertexArrayObject G.$= Just vertexArrayObject
  G.polygonMode G.$= (G.Line, G.Line)
  G.patchVertices G.$= 3
  G.pointSize G.$= 40.0

  G.maxPatchVertices >>= print

  G.deleteObjectNames [ fragmentShader
                      , tessellationControlShader
                      , tessellationEvaluationShader
                      , vertexShader ]

  return (program, vertexArrayObject)

render :: G.VertexArrayObject -> G.Program -> IO ()
render vao p = do
  G.clear [G.ColorBuffer]
  G.currentProgram G.$= Just p
  let offsetLocation = G.AttribLocation 0
      colorLocation = G.AttribLocation 1
  G.vertexAttrib4 offsetLocation (0.5 :: Float) 0.5 0.0 0.0
  G.vertexAttrib4 colorLocation (0.8 :: Float) 1.0 0.0 1.0
  G.drawArrays G.Patches 0 3

someFunc :: IO ()
someFunc = do
  graphicsInit
  withWindow
    (\win -> do
        G.makeContextCurrent $ Just win
        G.clearColor G.$= G.Color4 0.5 0 0 0
        G.viewport G.$= (G.Position 0 0, G.Size 800 600)

        printContextVersion win
        G.setWindowCloseCallback win (Just $ \w -> G.setWindowShouldClose w True)
        (prog, vertexArrayObject) <- compileShaders

        G.setWindowRefreshCallback win (Just $ \w -> do
                                           render vertexArrayObject prog
                                           G.swapBuffers w)
        loop win vertexArrayObject prog
        G.deleteObjectName vertexArrayObject
        G.deleteObjectName prog
        G.destroyWindow win
        G.terminate
    )
  where
    loop w vao p = do
      G.pollEvents
      sc <- G.windowShouldClose w
      unless sc $ loop w vao p
