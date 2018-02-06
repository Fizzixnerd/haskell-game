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
    Just win -> do
      f win
      G.destroyWindow win
      G.terminate

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
--  G.attachShader program tessellationControlShader
--  G.attachShader program tessellationEvaluationShader
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

render
  :: G.Program
     -> G.AttribLocation
     -> (Ptr CFloat, Int)
     -> (Ptr CUShort, Int)
     -> IO ()
render p posLocation locs (idxs, leni) = do
  G.clear [G.ColorBuffer]
  G.currentProgram G.$= Just p
  G.drawElements G.Triangles (fromIntegral leni) G.UnsignedShort idxs

loadObj :: MonadIO m => FilePath -> m W.WavefrontOBJ
loadObj fp = do
  eObj :: Either String W.WavefrontOBJ <- W.fromFile fp
  case eObj of
    Left e -> error e
    Right obj -> return obj

locationsToCFloats :: [W.Location] -> ([CFloat], Int)
locationsToCFloats xs = locationsToCFloats' xs 0
  where
    locationsToCFloats' [] n = ([], n)
    locationsToCFloats' (x:xs') n = ((CFloat $ W.locX x) : (CFloat $ W.locY x) : (CFloat $ W.locZ x) : (CFloat $ W.locW x) : fst ( locationsToCFloats' xs' (n + 4)), snd $ locationsToCFloats' xs' (n + 4))

indicesToUShorts :: [(Int, Int, Int)] -> ([CUShort], Int)
indicesToUShorts xs = indicesToUShorts' xs 0
  where
    indicesToUShorts' [] n = ([], n)
    indicesToUShorts' ((a, b, c):xs') n = (fromIntegral a : fromIntegral b : fromIntegral c : fst ( indicesToUShorts' xs' (n + 3)), snd $ indicesToUShorts' xs' (n + 3))

marshallLocations :: Vector W.Location -> IO (Ptr CFloat, Int)
marshallLocations locs = do
  a <- newArray floats
  return (a, n)
    where (floats, n) = locationsToCFloats $ toList locs

marshallIndices :: Vector (Int, Int, Int) -> IO (Ptr CUShort, Int)
marshallIndices idxs = do
  a <- newArray shorts
  return (a, n)
    where (shorts, n) = indicesToUShorts $ toList idxs

bufferData :: G.AttribLocation -> (Ptr CFloat, Int) -> IO ()
bufferData vtxLoc (vtxs, lenv) = do
  buf <- G.genObjectName
  G.bindBuffer G.ArrayBuffer G.$= Just buf
  G.bufferData G.ArrayBuffer G.$= (fromIntegral lenv, vtxs, G.StaticDraw)

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
        let posLocation = G.AttribLocation 0
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
                                render prog posLocation locs idxs
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
        G.deleteObjectName prog)
  where
    loop w = do
      G.pollEvents
      sc <- G.windowShouldClose w
      unless sc $ loop w
