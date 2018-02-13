{-# LANGUAGE NoImplicitPrelude#-}

module Game.Graphics.Shader.Loader where

import           ClassyPrelude
import           Game.Graphics.OpenGL.LowBinding
import           Text.Printf

makeShader :: MonadIO m => FilePath -> ShaderType -> m Shader
makeShader shaderName shaderType = liftIO $ do
  shaderText <- readFile shaderName
  shader <- createShader shaderType
  shaderSourceBS shader $= shaderText
  compileShader shader
  shaderInfoLog shader >>= (\x -> if null x then return () else printf "%s\n\n" x)
  return shader

compileShaders :: MonadIO m => m Program
compileShaders = liftIO $ do
  vertexShader <- makeShader "res/shaders/shader.vs" VertexShader
--  tessellationControlShader <- makeShader "res/shaders/shader.tcs" G.TessControlShader
--  tessellationEvaluationShader <- makeShader "res/shaders/shader.tes" G.TessEvaluationShader
--  geometryShader <- makeShader "res/shaders/shader.gs" G.GeometryShader
  fragmentShader <- makeShader "res/shaders/shader.fs" FragmentShader

  program <- createProgram
  attachShader program vertexShader
--  G.attachShader program tessellationControlShader
--  G.attachShader program tessellationEvaluationShader
--  G.attachShader program geometryShader
  attachShader program fragmentShader
  linkProgram program
  validateProgram program
  programInfoLog program >>= (\x -> if null x then return () else printf "%s\n\n" x)

  deleteObjectNames [ fragmentShader
--                      , tessellationControlShader
--                      , tessellationEvaluationShader
--                      , geometryShader
                      , vertexShader ]

  return program
