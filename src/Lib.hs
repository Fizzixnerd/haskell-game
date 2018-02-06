{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}

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
import qualified Linear as L
import Game.Types
import Data.Maybe
import GHC.Float (double2Float)

---
rotateCamera :: L.Quaternion Float -> Camera -> Camera
rotateCamera q cam = cam & cameraOrientation *~ q

translateCamera :: L.V3 Float -> Camera -> Camera
translateCamera v cam = cam & cameraPosition +~ v

mousePosToRot :: Double -> Double -> L.Quaternion Float
mousePosToRot x y = L.slerp xrot yrot 0.5
  where
    xrot = L.axisAngle (L.V3 1 0 0) (double2Float x)
    yrot = L.axisAngle (L.V3 0 1 0) (double2Float y)
---

graphicsInit :: MonadIO m => m ()
graphicsInit = liftIO $ do
  _ <- G.init
  G.windowHint $ G.WindowHint'ContextVersionMajor 4
  G.windowHint $ G.WindowHint'ContextVersionMinor 5
  G.windowHint $ G.WindowHint'OpenGLForwardCompat True
  G.windowHint $ G.WindowHint'Samples 4
  G.windowHint $ G.WindowHint'OpenGLProfile G.OpenGLProfile'Core
  G.windowHint $ G.WindowHint'OpenGLDebugContext True

withWindow :: MonadIO m => (G.Window -> m a) -> m a
withWindow f = do
  mwin <- liftIO $ G.createWindow 1920 1080 "Haskell Game Hello World" Nothing Nothing
  case mwin of
    Nothing -> error "Could not create window."
    Just win -> do
      x <- f win
      liftIO $ G.destroyWindow win
      return x

printContextVersion :: MonadIO m => G.Window -> m ()
printContextVersion win = liftIO $ do
  maj <- G.getWindowContextVersionMajor win
  min_ <- G.getWindowContextVersionMinor win
  rev <- G.getWindowContextVersionRevision win
  printf "%i.%i.%i\n" maj min_ rev

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

render :: MonadIO m =>
          G.Program
       -> G.AttribLocation
       -> G.VertexArrayObject
       -> G.BufferObject -- ^ Vertex buffer
       -> G.BufferObject -- ^ Element buffer
       -> (Ptr CFloat, Int)
       -> (Ptr CUShort, Int)
       -> m ()
render prog posLoc vao vbuf ebuf (vtxs, lenv) (idxs, leni) = liftIO $ do
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

marshallLocations :: MonadIO m => Vector W.Location -> m (Ptr CFloat, Int)
marshallLocations locs = liftIO $ do
  a <- newArray floats
  return (a, n)
    where (floats, n) = locationsToCFloats $ toList locs

marshallIndices :: MonadIO m => Vector (Int, Int, Int) -> m (Ptr CUShort, Int)
marshallIndices idxs = liftIO $ do
  a <- newArray shorts
  return (a, n)
    where (shorts, n) = indicesToUShorts $ toList idxs

bufferData :: MonadIO m =>
              G.AttribLocation
           -> (Ptr CUShort, Int)
           -> (Ptr CFloat, Int)
           -> m (G.BufferObject, G.BufferObject)
bufferData vtxLoc (idxs, leni) (vtxs, lenv) = liftIO $ do
  vbuf <- G.genObjectName
  G.bindBuffer G.ArrayBuffer G.$= Just vbuf
  G.bufferData G.ArrayBuffer G.$= ( fromIntegral $ lenv * (sizeOf (0.0 :: Float)) * 4
                                  , vtxs, G.StaticDraw )
  ebuf <- G.genObjectName
  G.bindBuffer G.ElementArrayBuffer G.$= Just ebuf
  G.bufferData G.ElementArrayBuffer G.$= ( fromIntegral $ leni * (sizeOf (0 :: CUShort)) * 3
                                         , idxs, G.StaticDraw )
  return (vbuf, ebuf)

doItandGimmeFireThing :: Game s (NamedHandler (), IO ())
doItandGimmeFireThing = do
  graphicsInit
  mWin <- liftIO $ G.createWindow 1920 1080 "Haskell Game Hello World" Nothing Nothing
  let win = fromJust mWin
  liftIO $ G.makeContextCurrent $ Just win

  -- get the Handlers we need.
  (addHandlerShouldClose, shouldClose) <- newNamedEventHandler "shouldClose"
  (addHandlerTick, tick) <- newNamedEventHandler "tick"
  (addHandlerKey, key) <- newNamedEventHandler "key"
  (addHandlerHello, hello) <- newNamedEventHandler "hello"
  (addHandlerMousePos, mousePos) <- newNamedEventHandler "mousePos"

  G.debugMessageCallback G.$= Just (printf "!!!%s!!!\n\n" . show)
  printContextVersion win
  liftIO $ G.setWindowCloseCallback win (Just $ fire shouldClose)

  prog <- liftIO $ compileShaders
  G.polygonMode G.$= (G.Line, G.Line)
  G.pointSize G.$= 40.0
  G.clearColor G.$= G.Color4 0.5 0 0 0
  G.viewport G.$= (G.Position 0 0, G.Size 1920 1080)

  liftIO $ G.setWindowRefreshCallback win (Just $ fire tick)
  liftIO $ G.setKeyCallback win (Just (\w k sc ks mk -> fire key (w, k, sc, ks, mk)))
  liftIO $ G.setCursorPosCallback win (Just (\w x y -> fire mousePos (w, x, y)))

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
        eMousePos <- B.fromAddHandler addHandlerMousePos

        let eClose :: B.Event (IO ())
            eClose = flip G.setWindowShouldClose True <$> eShouldClose

            eRender :: B.Event (IO ())
            eRender = (\w -> do
                          render prog posLocation vertexArrayObject vbuf ebuf locs idxs
                          G.swapBuffers w) <$> eTick

            eEscapeToClose :: B.Event (IO ())
            eEscapeToClose = (\(w, _, _, _, _) -> G.setWindowShouldClose w True)
              <$> B.filterE (\(_, k, _, _, _) -> k == G.Key'Escape) eKey

            ePrintHello :: B.Event (IO ())
            ePrintHello = (\_ -> print "hello") <$> eHello

        B.reactimate ePrintHello
        B.reactimate eClose
        B.reactimate eRender
        B.reactimate eEscapeToClose

  net <- liftIO $ B.compile network
  liftIO $ B.actuate net
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

  return (hello, someFunc)

someFunc :: Game s ()
someFunc = do
  x <- doItandGimmeFireThing
  liftIO $ snd x
