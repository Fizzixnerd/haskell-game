{-# LANGUAGE NoImplicitPrelude#-}

module Game.Graphics.Shader.Loader where

import           ClassyPrelude
import qualified Graphics.Rendering.OpenGL.GL as G
import           Text.Printf

makeShader :: MonadIO m => FilePath -> G.ShaderType -> m G.Shader
makeShader shaderName shaderType = liftIO $ do
  shaderText <- readFile shaderName
  shader <- G.createShader shaderType
  G.shaderSourceBS shader G.$= shaderText
  G.compileShader shader
  G.shaderInfoLog shader >>= (\x -> if null x then return () else printf "%s\n\n" x)
  return shader

compileShaders :: MonadIO m => m G.Program
compileShaders = liftIO $ do
  vertexShader <- makeShader "shader.vs" G.VertexShader
--  tessellationControlShader <- makeShader "shader.tcs" G.TessControlShader
--  tessellationEvaluationShader <- makeShader "shader.tes" G.TessEvaluationShader
--  geometryShader <- makeShader "shader.gs" G.GeometryShader
  fragmentShader <- makeShader "shader.fs" G.FragmentShader

  program <- G.createProgram
  G.attachShader program vertexShader
--  G.attachShader program tessellationControlShader
--  G.attachShader program tessellationEvaluationShader
--  G.attachShader program geometryShader
  G.attachShader program fragmentShader
  G.linkProgram program
  G.validateProgram program
  G.programInfoLog program >>= (\x -> if null x then return () else printf "%s\n\n" x)

  G.deleteObjectNames [ fragmentShader
--                      , tessellationControlShader
--                      , tessellationEvaluationShader
--                      , geometryShader
                      , vertexShader ]

  return program
