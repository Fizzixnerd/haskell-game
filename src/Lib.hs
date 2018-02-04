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
  G.init
  G.windowHint $ G.WindowHint'ContextVersionMajor 4
  G.windowHint $ G.WindowHint'ContextVersionMinor 5
  G.windowHint $ G.WindowHint'OpenGLProfile G.OpenGLProfile'Core

withWindow :: (G.Window -> IO ()) -> IO ()
withWindow f = do
  mwin <- G.createWindow 1920 1080 "Haskell Game Hello World" Nothing Nothing
  case mwin of
    Nothing -> error "Could not create window."
    Just win -> f win

printContextVersion :: G.Window -> IO ()
printContextVersion win = do
  maj <- G.getWindowContextVersionMajor win
  min <- G.getWindowContextVersionMinor win
  rev <- G.getWindowContextVersionRevision win
  printf "%i.%i.%i\n" maj min rev

compileShaders :: IO G.Program
compileShaders = do
  vertexShaderText <- readFile "shader.vs"
  fragmentShaderText <- readFile "shader.fs"

  vertexShader <- G.createShader G.VertexShader
  G.shaderSourceBS vertexShader G.$= vertexShaderText
  G.compileShader vertexShader

  fragmentShader <- G.createShader G.FragmentShader
  G.shaderSourceBS fragmentShader G.$= fragmentShaderText
  G.compileShader fragmentShader

  program <- G.createProgram
  G.attachShader program vertexShader
  G.attachShader program fragmentShader
  G.linkProgram program
  G.validateProgram program

  -- I guess I don't need to delete the shaders??
  
  return program

render :: G.VertexArrayObject -> G.Program -> IO ()
render vao p = do
  G.currentProgram G.$= (Just p)
  G.drawArrays G.Points 0 1

someFunc :: IO ()
someFunc = do
  graphicsInit
  withWindow 
    (\win -> do
        G.makeContextCurrent $ Just win
        printContextVersion win
        G.setWindowCloseCallback win (Just $ \w -> G.setWindowShouldClose w True)
        prog <- compileShaders
        G.programInfoLog prog >>= printf
        vertexArrayObject :: G.VertexArrayObject <- G.genObjectName
        G.bindVertexArrayObject G.$= (Just vertexArrayObject)
        G.setWindowRefreshCallback win (Just $ \w -> render vertexArrayObject prog)
        loop win vertexArrayObject prog)
  where
    loop w vao p = do
      G.pollEvents
      sc <- G.windowShouldClose w
      when sc $ do
        G.deleteObjectName vao
        G.destroyWindow w
        G.deleteObjectName p
        G.terminate
        exitSuccess
      G.swapBuffers w
      loop w vao p
