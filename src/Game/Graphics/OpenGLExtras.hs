module Graphics.Rendering.OpenGL.GL.Extras where

loadPic :: MonadIO m => FilePath -> m P.DynamicImage
loadPic fp = do
  bmpObj <- liftIO $ P.readImage fp
  case bmpObj of
    Left e -> error e
    Right obj -> return obj

marshallTextureBMP :: MonadIO m => P.DynamicImage -> m (ForeignPtr Word8, Int, Int)
marshallTextureBMP img = liftIO . return $ (a, w, h)
    where
      (P.ImageRGB8 (P.Image w h vimg)) = img
      (a, _, _) = VS.unsafeToForeignPtr vimg

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
    G.generateMipmap' G.Texture2D
    return tbuf
