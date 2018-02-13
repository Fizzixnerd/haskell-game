{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude#-}

module Game.Graphics.Shader.Loader where

import           ClassyPrelude
import           Game.Graphics.OpenGL.Binding
import           Text.Printf

makeShader :: (Shader t, MonadIO m) => FilePath -> m t
makeShader shaderPath = liftIO $ do
  shaderText <- readFile shaderPath
  shader <- genObjectName
  shaderSource shader $= shaderText
  mlog <- compileShader shader
  case mlog of
    Nothing -> return shader
    Just log' -> error $ printf "%s\n\n" (show log')

compileShaders :: MonadIO m => m Program
compileShaders = liftIO $ do
  (vertexShader :: VertexShader) <- makeShader "res/shaders/shader.vs"
--  tessellationControlShader <- makeShader "res/shaders/shader.tcs" G.TessControlShader
--  tessellationEvaluationShader <- makeShader "res/shaders/shader.tes" G.TessEvaluationShader
--  geometryShader <- makeShader "res/shaders/shader.gs" G.GeometryShader
  (fragmentShader :: FragmentShader) <- makeShader "res/shaders/shader.fs"

  program <- genObjectName
  attachShader program vertexShader
--  G.attachShader program tessellationControlShader
--  G.attachShader program tessellationEvaluationShader
--  G.attachShader program geometryShader
  attachShader program fragmentShader
  mlogL <- linkProgram program
  forM_ mlogL $ printf "%s\n\n" . show
  mlogV <- validateProgram program
  forM_ mlogV $ printf "%s\n\n" . show

  deleteObjectName fragmentShader
  deleteObjectName vertexShader
  return program
