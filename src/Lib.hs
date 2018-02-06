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
    Just win -> f win

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

render :: G.Program -> IO ()
render p = do
  G.clear [G.ColorBuffer]
  G.currentProgram G.$= Just p
  G.drawArrays G.Patches 0 3

loadObj :: MonadIO m => FilePath -> m W.WavefrontOBJ
loadObj fp = do
  eObj :: Either String W.WavefrontOBJ <- W.fromFile fp
  case eObj of
    Left e -> error e
    Right obj -> return obj

locationsToFloats :: [W.Location] -> ([Float], Int)
locationsToFloats xs = locationsToFloats' xs 0
  where
    locationsToFloats' [] n = ([], n)
    locationsToFloats' (x:xs') n = (W.locX x : W.locY x : W.locZ x : W.locW x : fst ( locationsToFloats' xs' (n + 4)), snd $ locationsToFloats' xs' (n + 4))

indicesToShorts :: [(Int, Int, Int)] -> ([CShort], Int)
indicesToShorts xs = indicesToShorts' xs 0
  where
    indicesToShorts' [] n = ([], n)
    indicesToShorts' ((a, b, c):xs') n = (fromIntegral a : fromIntegral b : fromIntegral c : fst ( indicesToShorts' xs' (n + 3)), snd $ indicesToShorts' xs' (n + 3))

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

drawSimple :: (Ptr Float, Int) -> (Ptr CShort, Int) -> IO ()
drawSimple (vtxs, lenv) (idxs, leni) = do
  G.drawElements G.Triangles (fromIntegral leni) G.Float idxs
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

        let network :: G.Program -> B.MomentIO ()
            network prog = mdo
              eTick <- B.fromAddHandler addHandlerTick
              eShouldClose <- B.fromAddHandler addHandlerShouldClose
              eKey <- B.fromAddHandler addHandlerKey

              let eClose :: B.Event (IO ())
                  eClose = flip G.setWindowShouldClose True <$> eShouldClose

                  eRender :: B.Event (IO ())
                  eRender = (\w -> do
                                render prog
                                G.swapBuffers w) <$> eTick

                  eEscapeToClose :: B.Event (IO ())
                  eEscapeToClose = (\(w, _, _, _, _) -> G.setWindowShouldClose w True)
                    <$> (B.filterE (\(_, k, _, _, _) -> k == G.Key'Escape) eKey)

              B.reactimate eClose
              B.reactimate eRender
              B.reactimate eEscapeToClose

        G.debugMessageCallback G.$= Just (printf "!!!%s!!!\n\n" . show)
        printContextVersion win
        G.setWindowCloseCallback win (Just fireShouldClose)

        prog <- compileShaders
        G.polygonMode G.$= (G.Line, G.Line)
        G.pointSize G.$= 5.0
        G.clearColor G.$= G.Color4 0.5 0 0 0
        G.viewport G.$= (G.Position 0 0, G.Size 1920 1080)

        vertexArrayObject :: G.VertexArrayObject <- G.genObjectName
        buf <- makeArrayBuffer
        let offsetLocation = G.AttribLocation 0
            colorLocation  = G.AttribLocation 1
        G.vertexAttrib4 offsetLocation (0.5 :: Float) 0.5 0.0 0.0
        G.vertexAttrib4 colorLocation (0.8 :: Float) 1.0 0.0 1.0
        G.bindVertexArrayObject G.$= Just vertexArrayObject
        G.setWindowRefreshCallback win (Just fireTick)
        G.setKeyCallback win (Just (\w k sc ks mk -> fireKey (w, k, sc, ks, mk)))

        obj <- loadObj "res/simple-cube.obj"
        let faces = (\W.Element {..} -> elValue) <$> W.objFaces obj
            faceIndices = (\(W.Face a b c xs) -> ( W.faceLocIndex a
                                                 , W.faceLocIndex b
                                                 , W.faceLocIndex c )) <$> faces
            locations = W.objLocations obj

            marshalledData = ( marshallLocations locations
                             , marshallIndices faceIndices )

        net <- B.compile $ network prog
        B.actuate net
        loop win
        G.deleteObjectName vertexArrayObject
        G.deleteObjectName prog
        G.deleteObjectName buf
        G.destroyWindow win
        G.terminate)
  where
    loop w = do
      G.pollEvents
      sc <- G.windowShouldClose w
      unless sc $ loop w
