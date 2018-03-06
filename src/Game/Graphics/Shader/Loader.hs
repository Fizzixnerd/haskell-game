{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude#-}

module Game.Graphics.Shader.Loader where

import           Game.Types
import           ClassyPrelude
import           Graphics.Binding
import           Text.Printf
import           Linear
import           Foreign.Resource

makeShader :: (Shader t, MonadIO m) => FilePath -> m t
makeShader shaderPath = liftIO $ do
  shaderText <- readFile shaderPath
  shader <- genName'
  shaderSource shader $= shaderText
  mlog <- compileShader shader
  case mlog of
    Nothing -> return shader
    Just log' -> error $ printf "%s\n\n" (show log')

compileShaders :: MonadIO m => m Program
compileShaders = liftIO $ do
  (vertexShader :: VertexShader) <- makeShader $
                                    "res" </> "shaders" </> "tween.vert"
--  tessellationControlShader <- makeShader "res/shaders/shader.tcs" G.TessControlShader
--  tessellationEvaluationShader <- makeShader "res/shaders/shader.tes" G.TessEvaluationShader
--  geometryShader <- makeShader "res/shaders/shader.gs" G.GeometryShader
  (fragmentShader :: FragmentShader) <- makeShader $
                                        "res" </> "shaders" </> "tween.frag"

  program <- genName'
  attachShader program vertexShader
--  G.attachShader program tessellationControlShader
--  G.attachShader program tessellationEvaluationShader
--  G.attachShader program geometryShader
  attachShader program fragmentShader
  mlogL <- linkProgram program
  forM_ mlogL $ printf "%s\n\n" . show
  mlogV <- validateProgram program
  forM_ mlogV $ printf "%s\n\n" . show

  deleteName fragmentShader
  deleteName vertexShader
  return program

data UniformMVP = UniformMVP deriving (Eq, Show, Ord)

instance Uniform UniformMVP where
  type UniformContents UniformMVP = M44 GLfloat
  type UniformLocationType UniformMVP = DefaultBlock
  uniform prg _ = primMarshal 0 prg
