{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import ClassyPrelude
import qualified Graphics.UI.GLFW as G
import qualified Graphics.Rendering.OpenGL.GL as G
import qualified Reactive.Banana.Combinators as B
import qualified Reactive.Banana.Frameworks as B
import qualified Codec.Wavefront as W
import Foreign.C.Types
import Foreign
import Text.Printf
import Data.Monoid (Sum(..))
import Control.Lens
import Data.Maybe

graphicsInit :: IO ()
graphicsInit = do
  _ <- G.init
  G.windowHint $ G.WindowHint'ContextVersionMajor 4
  G.windowHint $ G.WindowHint'ContextVersionMinor 5
  G.windowHint $ G.WindowHint'OpenGLForwardCompat True
  G.windowHint $ G.WindowHint'Samples 4
  G.windowHint $ G.WindowHint'OpenGLProfile G.OpenGLProfile'Core
  G.windowHint $ G.WindowHint'OpenGLDebugContext True

withWindow :: (G.Window -> IO a) -> IO a
withWindow f = do
  mwin <- G.createWindow 1920 1080 "Haskell Game Hello World" Nothing Nothing
  case mwin of
    Nothing -> error "Could not create window."
    Just win -> do
      x <- f win
      G.destroyWindow win
      return x

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

render :: G.Program
       -> G.AttribLocation
       -> G.VertexArrayObject
       -> G.BufferObject -- ^ Vertex buffer
       -> G.BufferObject -- ^ Element buffer
       -> (Ptr CFloat, Int)
       -> (Ptr CUShort, Int)
       -> IO ()
render prog posLoc vao vbuf ebuf (vtxs, lenv) (idxs, leni) = do
  G.clear [G.ColorBuffer]
  G.currentProgram G.$= Just prog
  G.bindVertexArrayObject G.$= Just vao
  G.vertexAttribArray posLoc G.$= G.Enabled
  G.bindBuffer G.ArrayBuffer G.$= Just vbuf
  let vad = G.VertexArrayDescriptor 3 G.Float 0 nullPtr
  G.vertexAttribPointer posLoc G.$= (G.ToFloat, vad)
  G.bindBuffer G.ElementArrayBuffer G.$= Just ebuf
  G.drawElements G.Triangles (fromIntegral leni `div` 3) G.UnsignedShort nullPtr

loadObj :: MonadIO m => FilePath -> m W.WavefrontOBJ
loadObj fp = do
  eObj :: Either String W.WavefrontOBJ <- W.fromFile fp
  case eObj of
    Left e -> error e
    Right obj -> return obj

locationsToCFloats :: [W.Location] -> ([CFloat], Int)
locationsToCFloats = (_2 %~ getSum) . foldMap go
  where
    go x = (fmap CFloat [W.locX x, W.locY x, W.locZ x, W.locW x], Sum 4)

indicesToUShorts :: [(Int, Int, Int)] -> ([CUShort], Int)
indicesToUShorts = (_2 %~ getSum) . foldMap go
  where
    go (a,b,c) = (fmap fromIntegral [a,b,c], Sum 3)

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

bufferData :: G.AttribLocation
           -> (Ptr CUShort, Int)
           -> (Ptr CFloat, Int)
           -> IO (G.BufferObject, G.BufferObject)
bufferData vtxLoc (idxs, leni) (vtxs, lenv) = do
  vbuf <- G.genObjectName
  G.bindBuffer G.ArrayBuffer G.$= Just vbuf
  G.bufferData G.ArrayBuffer G.$= ( fromIntegral $ lenv * (sizeOf (0.0 :: Float)) * 4
                                  , vtxs, G.StaticDraw )
  ebuf <- G.genObjectName
  G.bindBuffer G.ElementArrayBuffer G.$= Just ebuf
  G.bufferData G.ElementArrayBuffer G.$= ( fromIntegral $ leni * (sizeOf (0 :: CUShort)) * 3
                                         , idxs, G.StaticDraw )
  return (vbuf, ebuf)

doItandGimmeFireThing :: IO (B.Handler (), IO ())
doItandGimmeFireThing = do
  graphicsInit
  mWin <- G.createWindow 1920 1080 "Haskell Game Hello World" Nothing Nothing
  let win = fromJust mWin
  G.makeContextCurrent $ Just win

  -- get the Handlers we need.
  (addHandlerShouldClose, fireShouldClose) <- B.newAddHandler
  (addHandlerTick, fireTick) <- B.newAddHandler
  (addHandlerKey, fireKey) <- B.newAddHandler
  (addHandlerHello, fireHello :: () -> IO ()) <- B.newAddHandler

  G.debugMessageCallback G.$= Just (printf "!!!%s!!!\n\n" . show)
  printContextVersion win
  G.setWindowCloseCallback win (Just fireShouldClose)

  prog <- compileShaders
  G.polygonMode G.$= (G.Line, G.Line)
  G.pointSize G.$= 40.0
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
  (vbuf, ebuf) <- bufferData posLocation idxs locs

  let network :: B.MomentIO ()
      network = mdo
        eTick <- B.fromAddHandler addHandlerTick
        eShouldClose <- B.fromAddHandler addHandlerShouldClose
        eKey <- B.fromAddHandler addHandlerKey
        eHello <- B.fromAddHandler addHandlerHello

        let eClose :: B.Event (IO ())
            eClose = flip G.setWindowShouldClose True <$> eShouldClose

            eRender :: B.Event (IO ())
            eRender = (\w -> do
                          render prog posLocation vertexArrayObject vbuf ebuf locs idxs
                          G.swapBuffers w) <$> eTick

            eEscapeToClose :: B.Event (IO ())
            eEscapeToClose = (\(w, _, _, _, _) -> G.setWindowShouldClose w True)
              <$> (B.filterE (\(_, k, _, _, _) -> k == G.Key'Escape) eKey)

            ePrintHello :: B.Event (IO ())
            ePrintHello = (\_ -> print "hello") <$> eHello

        B.reactimate ePrintHello
        B.reactimate eClose
        B.reactimate eRender
        B.reactimate eEscapeToClose

  net <- B.compile network
  B.actuate net
  let someFunc :: IO ()
      someFunc = do
        loop win
        G.deleteObjectName vertexArrayObject
        G.deleteObjectName prog
        G.terminate
          where
            loop w = do
              G.pollEvents
              sc <- G.windowShouldClose w
              unless sc $ loop w
        
  return (fireHello, someFunc)

someFunc :: IO ()
someFunc = do
  x <- doItandGimmeFireThing
  snd x
