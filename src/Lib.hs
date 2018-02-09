{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import ClassyPrelude
import Control.Concurrent
import Control.Lens
import Data.Maybe
import Data.Monoid (Sum(..))
import Foreign hiding (void)
import Foreign.C.Types
import GHC.Float (double2Float)
import Text.Printf

import Game.Types

import qualified Graphics.UI.GLFW as G
import qualified Graphics.Rendering.OpenGL.GL as G
import qualified Reactive.Banana.Combinators as B
import qualified Reactive.Banana.Frameworks as B
import qualified Codec.Wavefront as W
import qualified Linear as L
import qualified Linear.OpenGL as L ()
import qualified Codec.Picture as P
import qualified Data.Vector.Storable as VStor (unsafeToForeignPtr)
import qualified Graphics.Text.TrueType as T
import qualified Codec.Picture as T
import qualified Graphics.Rasterific as T
import qualified Graphics.Rasterific.Texture as T

rotateCamera :: (Float, Float) -> Camera -> Camera
rotateCamera (dhor, dver) cam = cam & cameraOrientation %~ go
  where
    go (hor, ver) = (hor + dhor, max (-pi/2) . min (pi/2) $ ver + dver)

translateCameraRelative :: L.V3 Float -> Camera -> Camera
translateCameraRelative v cam = cam & cameraPosition +~ vrel
  where
    vrel = L.rotate (L.axisAngle (L.V3 0 1 0) (fst . _cameraOrientation $ cam)) v

mousePosToRot :: Float -> Double -> Double -> (Float, Float)
mousePosToRot mouseSpeed x y = (xrot, yrot)
  where
    xrot = mouseSpeed * double2Float (1920/2 - x)
    yrot = mouseSpeed * double2Float (1080/2 - y)

moveCamera :: Float -> Float -> Movement -> Camera -> Camera
moveCamera _ t MoveLeft            = translateCameraRelative (L.V3 (negate t) 0 0)
moveCamera _ t MoveRight           = translateCameraRelative (L.V3 t 0 0)
moveCamera _ t MoveForward         = translateCameraRelative (L.V3 0 0 (negate t))
moveCamera _ t MoveBackward        = translateCameraRelative (L.V3 0 0 t)
moveCamera _ t MoveUp              = translateCameraRelative (L.V3 0 t 0)
moveCamera _ t MoveDown            = translateCameraRelative (L.V3 0 (negate t) 0)
moveCamera o _ (MoveCameraDir x y) = rotateCamera q
  where
    q = mousePosToRot o x y

keyToMovement :: G.Key -> Maybe Movement
keyToMovement G.Key'W = Just MoveForward
keyToMovement G.Key'A = Just MoveLeft
keyToMovement G.Key'S = Just MoveBackward
keyToMovement G.Key'D = Just MoveRight
keyToMovement G.Key'LeftShift = Just MoveDown
keyToMovement G.Key'Space = Just MoveUp
keyToMovement _ = Nothing

mouseToMovement :: (G.Window, Double, Double) -> Movement
mouseToMovement (_, x, y) = MoveCameraDir x y

cameraMVP :: Getter Camera (L.M44 Float)
cameraMVP = to go
  where
    go (Camera vpos (vangh, vangv) cfov) = camPerspective L.!*! camView L.!*! camModel
      where
        vup  = L.V3 0 1 0
        vdir = L.rotate (L.axisAngle (L.V3 0 1 0) vangh * L.axisAngle (L.V3 1 0 0) vangv) (L.V3 0 0 (negate 1))
        camModel = L.identity
        camView = L.lookAt vpos (vpos + vdir) vup
      -- Projection matrix : 90deg Field of View, 16:9 ratio, display range : 0.1 unit <-> 100 units
        camPerspective = L.perspective cfov (16/9) 0.1 100

graphicsInit :: MonadIO m => m ()
graphicsInit = liftIO $ do
  _ <- G.init
  G.windowHint $ G.WindowHint'ContextVersionMajor 4
  G.windowHint $ G.WindowHint'ContextVersionMinor 5
  G.windowHint $ G.WindowHint'OpenGLForwardCompat True
  G.windowHint $ G.WindowHint'Samples 4
  G.windowHint $ G.WindowHint'OpenGLProfile G.OpenGLProfile'Core
  G.windowHint $ G.WindowHint'OpenGLDebugContext True
  G.windowHint $ G.WindowHint'DepthBits 24
--  G.cullFace G.$= Just G.Front
  G.depthFunc G.$= Just G.Less

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

render :: MonadIO m =>
          GameState
       -> G.Program
       -> G.UniformLocation
       -> G.UniformLocation
       -> G.AttribLocation
       -> G.VertexArrayObject
       -> G.BufferObject -- ^ Vertex buffer
       -> G.BufferObject -- ^ Texture coordinate buffer
       -> G.BufferObject -- ^ Element buffer
       -> (Ptr CFloat, Int)
       -> (Ptr CUInt, Int)
       -> G.TextureObject
       -> m ()
render gs prog mvpLoc texSampleLocation _ vao vbuf tbuf ebuf _ (_, leni) tex = liftIO $ do
  G.clear [G.ColorBuffer, G.DepthBuffer]
  G.currentProgram G.$= Just prog

  G.activeTexture G.$= G.TextureUnit 0
  G.textureBinding G.Texture2D G.$= Just tex
  G.uniform texSampleLocation G.$= G.TextureUnit 0

  G.uniform mvpLoc G.$= (gs ^. gameStateCamera . cameraMVP)
  G.bindVertexArrayObject G.$= Just vao
--  G.bindBuffer G.ArrayBuffer G.$= Just vbuf
--  G.bindBuffer G.TextureBuffer G.$= Just tbuf
--  G.bindBuffer G.ElementArrayBuffer G.$= Just ebuf
  G.drawElements G.Triangles (fromIntegral leni) G.UnsignedInt nullPtr

loadObj :: MonadIO m => FilePath -> m W.WavefrontOBJ
loadObj fp = do
  eObj :: Either String W.WavefrontOBJ <- W.fromFile fp
  case eObj of
    Left e -> error e
    Right obj -> return obj

loadPic :: MonadIO m => FilePath -> m P.DynamicImage
loadPic fp = do
  bmpObj <- liftIO $ P.readImage fp
  case bmpObj of
    Left e -> error e
    Right obj -> return obj

locationsToCFloats :: [W.Location] -> ([CFloat], Int)
locationsToCFloats = (_2 %~ getSum) . foldMap go
  where
    go x = (fmap CFloat [W.locX x, W.locY x, W.locZ x, W.locW x], Sum 4)

indicesToUInts :: [(Int, Int, Int)] -> ([CUInt], Int)
indicesToUInts = (_2 %~ getSum) . foldMap go
  where
    go (a,b,c) = (fmap fromIntegral [a,b,c], Sum 3)

texCoordsToCFloats :: [W.TexCoord] -> ([CFloat], Int)
texCoordsToCFloats = (_2 %~ getSum) . foldMap go
  where
    go x = (fmap CFloat [W.texcoordR x, W.texcoordS x], Sum 2)

marshallLocations :: MonadIO m => Vector W.Location -> m (Ptr CFloat, Int)
marshallLocations locs = liftIO $ do
  a <- newArray floats
  return (a, n)
    where (floats, n) = locationsToCFloats $ toList locs

marshallIndices :: MonadIO m => Vector (Int, Int, Int) -> m (Ptr CUInt, Int)
marshallIndices idxs = liftIO $ do
  a <- newArray shorts
  return (a, n)
    where (shorts, n) = indicesToUInts $ toList idxs

marshallTexCoords :: MonadIO m => Vector W.TexCoord -> m (Ptr CFloat, Int)
marshallTexCoords texcoords = liftIO $ do
  a <- newArray floats
  return (a, n)
    where (floats, n) = texCoordsToCFloats $ toList texcoords

{-
marshallTextureBMP :: MonadIO m => P.DynamicImage -> m (Ptr Word8, Int, Int)
marshallTextureBMP img = liftIO $ do
  a <- newArray floats
  return (a, w, h)
    where
      (P.ImageRGB8 (P.Image w h vimg)) = img
      floats = toList vimg
-}

marshallTextureBMP :: MonadIO m => P.DynamicImage -> m (ForeignPtr Word8, Int, Int)
marshallTextureBMP img = liftIO . return $ (a, w, h)
    where
      (P.ImageRGB8 (P.Image w h vimg)) = img
      (a, _, _) = VStor.unsafeToForeignPtr vimg

loadBMPTexture :: MonadIO m => FilePath -> m G.TextureObject
loadBMPTexture fp = liftIO $ do
  (ufptr, w, h) <- loadPic fp >>= marshallTextureBMP
  tbuf <- G.genObjectName
  G.textureBinding G.Texture2D G.$= Just tbuf
  withForeignPtr ufptr $ \texptr -> do
    let texSize = G.TextureSize2D (fromIntegral w) (fromIntegral h)
        pixDat  = G.PixelData G.RGB G.UnsignedByte texptr
        minFilter = (G.Nearest, Nothing)
        magFilter = G.Nearest
    G.texImage2D G.Texture2D G.NoProxy 0 G.RGB8 texSize 0 pixDat
    G.textureFilter G.Texture2D G.$= (minFilter, magFilter)
    return tbuf

bufferData :: MonadIO m =>
              G.AttribLocation
           -> (Ptr CFloat, Int)
           -> G.AttribLocation
           -> (Ptr CFloat, Int)
           -> (Ptr CUInt, Int)
           -> m (G.VertexArrayObject, G.BufferObject, G.BufferObject, G.BufferObject)
bufferData vtxLoc (vtxs, lenv) texLoc (texs, lent) (idxs, leni) = liftIO $ do
  vao <- G.genObjectName
  G.bindVertexArrayObject G.$= Just vao

  vbuf <- G.genObjectName
  G.vertexAttribArray vtxLoc G.$= G.Enabled
  G.bindBuffer G.ArrayBuffer G.$= Just vbuf
  G.bufferData G.ArrayBuffer G.$= ( fromIntegral $ lenv * sizeOf (0.0 :: CFloat)
                                  , vtxs
                                  , G.StaticDraw )
  let vad = G.VertexArrayDescriptor 4 G.Float 0 nullPtr
  G.vertexAttribPointer vtxLoc G.$= (G.ToFloat, vad)


  tbuf <- G.genObjectName
  G.vertexAttribArray texLoc G.$= G.Enabled
  G.bindBuffer G.ArrayBuffer G.$= Just tbuf
  G.bufferData G.ArrayBuffer G.$= ( fromIntegral $ lent * sizeOf (0.0 :: CFloat)
                                  , texs
                                  , G.StaticDraw )
  let tad = G.VertexArrayDescriptor 2 G.Float 0 nullPtr
  G.vertexAttribPointer texLoc G.$= (G.ToFloat, tad)

  ebuf <- G.genObjectName
  G.bindBuffer G.ElementArrayBuffer G.$= Just ebuf
  G.bufferData G.ElementArrayBuffer G.$= ( fromIntegral $ leni * sizeOf (0 :: CUInt)
                                         , idxs
                                         , G.StaticDraw )
  free vtxs
  free idxs
  free texs
  return (vao, vbuf, tbuf, ebuf)

doItAndGimmeFireThing :: Game ( NamedHandler ()
                              , NamedHandler ()
                              , NamedHandler G.Window
                              , G.Window
                              , IO () )
doItAndGimmeFireThing = do
  graphicsInit
  mWin <- liftIO $ G.createWindow 1920 1080 "Haskell Game Hello World" Nothing Nothing
  let win = fromJust mWin
  liftIO $ G.makeContextCurrent $ Just win

  liftIO $ G.setCursorInputMode win G.CursorInputMode'Disabled

  eFont <- liftIO $ T.loadFontFile "fonts/comic-sans.ttf"
  let font = case eFont of
        Left e -> error e
        Right f -> f
      text = T.renderDrawing 300 70 (T.PixelRGBA8 255 255 255 255)
             . T.withTexture (T.uniformTexture $ T.PixelRGBA8 0 0 0 255) $
             T.printTextAt font (T.PointSize 12) (T.V2 20 40)
             "A simple text test!"

  -- get the Handlers we need.
  (addHandlerShouldClose, shouldClose) <- newNamedEventHandler "shouldClose"
  (addHandlerTick, tick) <- newNamedEventHandler "tick"
  (addHandlerKey, key) <- newNamedEventHandler "key"
  (addHandlerHello, hello) <- newNamedEventHandler "hello"
  (addHandlerMousePos, mousePos) <- newNamedEventHandler "mousePos"
  (addHandlerGameReset, gameReset) <- newNamedEventHandler "gameReset" 

  G.debugMessageCallback G.$= Just (printf "!!!%s!!!\n\n" . show)
  printContextVersion win
  liftIO $ G.setWindowCloseCallback win (Just $ fire shouldClose)

  prog <- liftIO compileShaders
  -- This is the bad way of doing this.
  mvpLocation <- liftIO $ G.uniformLocation prog "MVP"
  texSampleLocation <- liftIO $ G.uniformLocation prog "texSampler"

--  G.polygonMode G.$= (G.Line, G.Line)
--  G.pointSize G.$= 40.0
  G.clearColor G.$= G.Color4 0 0 0.4 0
  -- G.viewport G.$= (G.Position 0 0, G.Size 1920 1080)

  liftIO $ G.setKeyCallback win (Just (\w k sc ks mk -> fire key (w, k, sc, ks, mk)))
  liftIO $ G.setCursorPosCallback win (Just (\w x y -> fire mousePos (w, x, y) >> G.setCursorPos w (1920 / 2) (1080 / 2)))

  obj <- loadObj "res/male.obj"
  let faces = (\W.Element {..} -> elValue) <$> W.objFaces obj
      faceIndices = (\(W.Face a b c _) -> ( W.faceLocIndex a - 1
                                          , W.faceLocIndex b - 1
                                          , W.faceLocIndex c - 1)) <$> faces
      locations = W.objLocations obj
      textureCoords = W.objTexCoords obj
  locs <- marshallLocations locations
  texs <- marshallTexCoords textureCoords
  idxs <- marshallIndices faceIndices

  tex <- loadBMPTexture "res/simple-cube-2.bmp"

  let posLocation = G.AttribLocation 0
      texLocation  = G.AttribLocation 1
  (vertexArrayObject, vbuf, tbuf, ebuf) <- bufferData posLocation locs texLocation texs idxs

  let network :: B.MomentIO ()
      network = mdo
        let initGameState = GameState
              { _gameStateCamera = Camera
                { _cameraPosition = L.V3 0 0 0
                , _cameraOrientation = (0, 0)
                , _cameraFOV = pi/2 } }
        eTick <- B.fromAddHandler addHandlerTick
        eShouldClose <- B.fromAddHandler addHandlerShouldClose
        eKey <- B.fromAddHandler addHandlerKey
        eHello <- B.fromAddHandler addHandlerHello
        eMousePos <- B.fromAddHandler addHandlerMousePos
        eGameReset <- B.fromAddHandler addHandlerGameReset

        let eClose :: B.Event (IO ())
            eClose = flip G.setWindowShouldClose True <$> eShouldClose

            eRender :: B.Event (IO ())
            eRender = B.apply ((\gs w -> do
                                   render gs prog mvpLocation texSampleLocation posLocation vertexArrayObject vbuf tbuf ebuf locs idxs tex
                                   G.swapBuffers w) <$> bWorld) eTick

            eEscapeToClose :: B.Event (IO ())
            eEscapeToClose = (\(w, _, _, _, _) -> G.setWindowShouldClose w True)
              <$> B.filterE (\(_, k, _, _, _) -> k == G.Key'Escape) eKey

            ePrintHello :: B.Event (IO ())
            ePrintHello = const (print ("hello" :: String)) <$> eHello

            eMovement :: B.Event Movement
            eMovement = B.unionWith const (B.filterJust ((\(_,k,_,_,_) -> keyToMovement k) <$> eKey)) (mouseToMovement <$> eMousePos)

            eCamMove :: B.Event (GameState -> GameState)
            eCamMove = (\m gs -> (gameStateCamera %~ moveCamera (0.005/60) (3/60) m $ gs)) <$> eMovement
            
            eResetGame :: B.Event (GameState -> GameState)
            eResetGame = const (const initGameState) <$> eGameReset

        bWorld <- B.accumB initGameState (B.unions [eCamMove, eResetGame])

        B.reactimate ePrintHello
        B.reactimate eClose
        B.reactimate eRender
        B.reactimate eEscapeToClose

  net <- liftIO $ B.compile network
  liftIO $ B.actuate net
  let someFunc' :: IO ()
      someFunc' = do
        loop win
        G.deleteObjectName vertexArrayObject
        G.deleteObjectName prog
        G.terminate
          where
            loop w = do
              fire tick w
              G.pollEvents
              sc <- G.windowShouldClose w
              unless sc $ loop w

  return (hello, gameReset, shouldClose, win, someFunc')

game :: Game ( NamedHandler ()
             , NamedHandler ()
             , IO ()
             , ThreadId)
game = do
  (h, r, sc, w, x) <- doItAndGimmeFireThing
  ti <- liftIO $ forkIO x
  return (h, r, fire sc w, ti)

someFunc :: IO ()
someFunc = void $ runGame game
