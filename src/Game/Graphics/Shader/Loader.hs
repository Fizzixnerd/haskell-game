{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude#-}
{-# LANGUAGE FlexibleContexts #-}

module Game.Graphics.Shader.Loader where

import           ClassyPrelude
import           Graphics.Binding
import           Text.Printf
import           Foreign.Resource

makeShader :: (ShaderType t, MonadIO m) => FilePath -> m (ShaderStage t)
makeShader shaderPath = do
  shaderText <- liftIO $ readFile shaderPath
  shader <- genName shaderText
  mlog <- readR GLInfoLog shader
  case mlog of
    Nothing -> do
      mlog' <- readR GLValidateStatus shader
      case mlog' of
        Nothing -> return shader
        Just log'' -> error $ printf "%s\n\n" (show log'')
    Just log' -> error $ printf "%s\n\n" (show log')

loadPipeline :: MonadIO m =>
  FilePath -> m (ShaderPipeline, ShaderStage 'VertexShader, ShaderStage 'FragmentShader)
loadPipeline shaderName = liftIO $ do
  vertexShader <- makeShader $
                  "res" </> "shaders" </> shaderName <.> "vert"
  fragmentShader <- makeShader $
                    "res" </> "shaders" </> shaderName <.> "frag"

  pipeline <- genName'
  pipeline $= ShaderPipelineSpec vertexShader Nothing Nothing Nothing fragmentShader
  mlogV <- readR GLValidateStatus pipeline
  forM_ mlogV $ printf "%s\n\n" . show

  return (pipeline, vertexShader, fragmentShader)

compilePipeline :: MonadIO m => m ( (ShaderPipeline, ShaderStage 'VertexShader, ShaderStage 'FragmentShader)
                                  , (ShaderPipeline, ShaderStage 'VertexShader, ShaderStage 'FragmentShader) )
compilePipeline = liftIO $ do
  pipelinePhong <- loadPipeline "phong"
  pipelineNormalMap <- loadPipeline "normalmap"
  return (pipelinePhong, pipelineNormalMap)
