{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
    ( someFunc
    ) where

import ClassyPrelude
import qualified Graphics.UI.GLFW as G
import qualified Graphics.Rendering.OpenGL.GL as G
import qualified Reactive.Banana.Combinators as B
import qualified Reactive.Banana.Frameworks as B
import qualified Codec.Wavefront as W
import Foreign.C.Types
import Foreign
import Text.Printf
import Data.Monoid ((<>), Sum(..))
import Control.Lens

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
  mwin <- G.createWindow 1920 1080 "Haskell Game Hello World" Nothing Nothing
  case mwin of
    Nothing -> error "Could not create window."
    Just win -> do
      f win
      G.destroyWindow win

printContextVersion :: G.Window -> IO ()
printContextVersion win = do
  maj <- G.getWindowContextVersionMajor win
  min_ <- G.getWindowContextVersionMinor win
  rev <- G.getWindowContextVersionRevision win
  printf "%i.%i.%i\n" maj min_ rev

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

makeArrayBuffer :: IO G.BufferObject
makeArrayBuffer = do
  buf :: G.BufferObject <- G.genObjectName
  G.bindBuffer G.ArrayBuffer G.$= Just buf
  with (69420 :: Int)
    (\ptr -> do
        G.bufferData G.ArrayBuffer G.$= ( CPtrdiff (fromIntegral $ sizeOf (0 :: Int))
                                        , ptr
                                        , G.StreamDraw )
        return buf)

render
  :: G.Program
     -> G.AttribLocation
     -> (Ptr Float, Int)
     -> (Ptr CShort, Int)
     -> IO ()
render p offsetLocation locs idxs = do
  G.clear [G.ColorBuffer]
  G.currentProgram G.$= Just p
  drawSimple offsetLocation locs idxs

loadObj :: MonadIO m => FilePath -> m W.WavefrontOBJ
loadObj fp = do
  eObj :: Either String W.WavefrontOBJ <- W.fromFile fp
  case eObj of
    Left e -> error e
    Right obj -> return obj

locationsToFloats :: Integral a => [W.Location] -> ([Float], a)
locationsToFloats = (_2 %~ getSum) . foldMap go
  where
    go x = ([W.locX x, W.locY x, W.locZ x, W.locW x], Sum 4)

indicesToShorts :: (Integral a, Integral b, Integral c) => [(a, a, a)] -> ([b], c)
indicesToShorts = (_2 %~ getSum) . foldMap go
  where
    go (a,b,c) = (fmap fromIntegral [a,b,c], Sum 3)

marshallLocations :: Vector W.Location -> IO (Ptr Float, Int)
marshallLocations locs = do
  a <- newArray floats
  return (a, n)
    where (floats, n) = locationsToFloats $ toList locs

marshallIndices :: Vector (Int, Int, Int) -> IO (Ptr CShort, Int)
marshallIndices idxs = do
  a <- newArray shorts
  return (a, n)
    where (shorts, n) = indicesToShorts $ toList idxs

drawSimple :: G.AttribLocation -> (Ptr Float, Int) -> (Ptr CShort, Int) -> IO ()
drawSimple attrLoc (vtxs, lenv) (idxs, leni) = do
  let vad = G.VertexArrayDescriptor (fromIntegral lenv) G.Float (fromIntegral $ sizeOf (0.0 :: Float)) vtxs
  G.vertexAttribPointer attrLoc G.$= (G.ToFloat, vad)
  G.vertexAttribArray attrLoc G.$= G.Enabled
  G.drawElements G.Patches (fromIntegral leni) G.Short idxs
  return ()

someFunc :: IO ()
someFunc = do
  graphicsInit
  withWindow
    (\win -> do
        G.makeContextCurrent $ Just win

        -- get the Handlers we need.
        (addHandlerShouldClose, fireShouldClose) <- B.newAddHandler
        (addHandlerTick, fireTick) <- B.newAddHandler
        (addHandlerKey, fireKey) <- B.newAddHandler

        G.debugMessageCallback G.$= Just (printf "!!!%s!!!\n\n" . show)
        printContextVersion win
        G.setWindowCloseCallback win (Just fireShouldClose)

        prog <- compileShaders
        G.polygonMode G.$= (G.Line, G.Line)
        G.pointSize G.$= 5.0
        G.clearColor G.$= G.Color4 0.5 0 0 0
        G.viewport G.$= (G.Position 0 0, G.Size 1920 1080)

        G.setWindowRefreshCallback win (Just fireTick)
        G.setKeyCallback win (Just (\w k sc ks mk -> fireKey (w, k, sc, ks, mk)))

        obj <- loadObj "res/simple-cube.obj"
        let faces = (\W.Element {..} -> elValue) <$> W.objFaces obj
            faceIndices = (\(W.Face a b c xs) -> ( W.faceLocIndex a
                                                 , W.faceLocIndex b
                                                 , W.faceLocIndex c )) <$> faces
            locations = W.objLocations obj

        locs <- marshallLocations locations
        idxs <- marshallIndices faceIndices 

        vertexArrayObject <- G.genObjectName
        buf <- makeArrayBuffer
        let offsetLocation = G.AttribLocation 0
            colorLocation  = G.AttribLocation 1
        G.vertexAttrib4 colorLocation (0.8 :: Float) 1.0 0.0 1.0
        G.bindVertexArrayObject G.$= Just vertexArrayObject

        let network :: G.Program -> B.MomentIO ()
            network prog = mdo
              eTick <- B.fromAddHandler addHandlerTick
              eShouldClose <- B.fromAddHandler addHandlerShouldClose
              eKey <- B.fromAddHandler addHandlerKey

              let eClose :: B.Event (IO ())
                  eClose = flip G.setWindowShouldClose True <$> eShouldClose

                  eRender :: B.Event (IO ())
                  eRender = (\w -> do
                                render prog offsetLocation locs idxs
                                G.swapBuffers w) <$> eTick

                  eEscapeToClose :: B.Event (IO ())
                  eEscapeToClose = (\(w, _, _, _, _) -> G.setWindowShouldClose w True)
                    <$> (B.filterE (\(_, k, _, _, _) -> k == G.Key'Escape) eKey)

              B.reactimate eClose
              B.reactimate eRender
              B.reactimate eEscapeToClose

        net <- B.compile $ network prog
        B.actuate net
        loop win
        G.deleteObjectName vertexArrayObject
        G.deleteObjectName prog
        G.deleteObjectName buf
        G.terminate)
  where
    loop w = do
      G.pollEvents
      sc <- G.windowShouldClose w
      unless sc $ loop w
