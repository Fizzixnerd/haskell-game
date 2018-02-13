{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Game.Graphics.OpenGL.LowBinding
  ( module X
  , genBufferObject
  , initBufferObject
  , bufferSubData
  , mapBuffer
  , unmapBuffer
  , clearBufferSubData
  , copyBufferSubData
  , bindVertexArrayObject
  , vertexArrayAttribBinding
  , vertexArrayVertexBuffer
  , vertexArrayAttribFormat
  , vertexArrayAttribEnable
  , vertexArrayAttribDisable
  , mapBufferRange
  , bindTextureUnit
  , toBufferObjectOffset
  , toBufferObjectSize
  , toBufferObjectStride
  , vertexArrayElementBuffer
  , textureParameterf
  , textureParameteri
  , compileShader
  , shaderDeleteStatus
  , shaderInfoLog
  , shaderSource
  , programDeleteStatus
  , attachShader
  , linkProgram
  , validateProgram
  , useProgram
  ) where

import Graphics.Rendering.OpenGL.GL as X
  ( MappingFailure(..)
  , UniformLocation(..)
  , VariableType(..)
  , Uniform(..)
  , uniform
  , uniformv
  , ($=)
  , clear
  , ClearBuffer(..)
  , drawElements
  , PrimitiveMode(..)
  )

import Game.Graphics.OpenGL.LowTypes as X
  ( BufferObjectMapType(..)
  , BufferObjectSize(..)
  , objectSize
  , BufferObjectOffset(..)
  , objectOffset
  , BufferObjectRelOffset(..)
  , objectRelOffset
  , BufferObjectStride(..)
  , objectStride
  , BufferObjectComponentSize(..)
  , objectComponentSize
  , TextureTarget(..)
  , createTexture
  , TextureTarget1D(..)
  , TextureTarget2D(..)
  , TextureTarget3D(..)
  , Texture1DAttrib(..)
  , Texture2DAttrib(..)
  , Texture3DAttrib(..)
  , Pixel1DAttrib(..)
  , Pixel2DAttrib(..)
  , Pixel3DAttrib(..)
  , mipmapLevel
  , bufferFormat
  , pixelFormat
  , bufferWidth
  , bufferHeight
  , bufferDepth
  , SizedFormat(..)
  , defaultBufferAttribFlags
  , IntegerHandling(..)
  , mapType
  , mapPersistent
  , mapCoherent
  , mapInvalidateRange
  , mapInvalidateBuffer
  , mapFlushExplicit
  , mapUnsynchronized
  , mapDynamic
  , clientStorage
  , PixelFormat(..)
  , TextureParameter(..)
  , GLDataType(..)
  , TextureObject
  , VertexShader
  , TessEvalShader
  , TessControlShader
  , FragmentShader
  , ComputeShader
  , Shader
  , Program
  , GeneratableObjectName(..)
  , ObjectName(..)
  , isObjectName
  , deleteObjectName
  , genObjectName
  , genObjectNames
  , genTextureName
  , genTextureNames
  , TextureUnit(..)
  , VertexArrayObject
  , AttribLocation(..)
  , BufferObject
  )

import Linear.OpenGL ()
import ClassyPrelude
import Foreign.Ptr
import Game.Graphics.OpenGL.LowTypes
import Graphics.GL.Core45
import Graphics.GL.Types
import Data.Bits ((.|.))
import Game.Graphics.OpenGL.Utils
import Data.StateVar as X
import Foreign.Marshal.Utils

{-
{-# INLINE finallyRet #-}
finallyRet :: (MonadMask m, MonadIO m) => m a -> m b -> m (a, b)
a `finallyRet` sequel = do
   r2Ref <- liftIO $ newIORef undefined
   r1 <- CE.finally a $ do
     x <- sequel
     liftIO $ writeIORef r2Ref x
   r2 <- liftIO $ readIORef r2Ref
   return (r1, r2)
-}

maybeNullPtr :: b -> (Ptr a -> b) -> Ptr a -> b
maybeNullPtr n f ptr | ptr == nullPtr = n
                     | otherwise      = f ptr

marshallBufferObjectAttribFlags :: BufferObjectAttribFlags -> GLbitfield
marshallBufferObjectAttribFlags BufferObjectAttribFlags {..}
  = getBitOr . concatMap BitOr $
    [bdyna, brewr, bpers, bcohe, bclie]
  where
    brewr = case _bufferObjectAttribFlagsMapType of
      MapNone -> 0
      MapRead -> GL_MAP_READ_BIT
      MapWrite -> GL_MAP_WRITE_BIT
      MapReadWrite -> GL_MAP_READ_BIT .|. GL_MAP_WRITE_BIT
    bpers = if _bufferObjectAttribFlagsMapPersistent then GL_MAP_PERSISTENT_BIT else 0
    bcohe = if _bufferObjectAttribFlagsMapCoherent then GL_MAP_COHERENT_BIT else 0
    bdyna = if _bufferObjectAttribFlagsMapDynamic then GL_DYNAMIC_STORAGE_BIT else 0
    bclie = if _bufferObjectAttribFlagsClientStorage then GL_CLIENT_STORAGE_BIT else 0

marshallBufferObjectMapFlags :: BufferObjectMapFlags -> GLbitfield
marshallBufferObjectMapFlags BufferObjectMapFlags {..}
  = getBitOr . concatMap BitOr $
  [brewr, bpers, bcohe, bira, bibu, bfle, bmun]
  where
    brewr = case _bufferObjectMapFlagsMapType of
      MapNone -> 0
      MapRead -> GL_MAP_READ_BIT
      MapWrite -> GL_MAP_WRITE_BIT
      MapReadWrite -> GL_MAP_READ_BIT .|. GL_MAP_WRITE_BIT
    bpers = if _bufferObjectMapFlagsMapPersistent then GL_MAP_PERSISTENT_BIT else 0
    bcohe = if _bufferObjectMapFlagsMapCoherent then GL_MAP_COHERENT_BIT else 0
    bira  = if _bufferObjectMapFlagsMapInvalidateRange then GL_MAP_INVALIDATE_RANGE_BIT else 0
    bibu  = if _bufferObjectMapFlagsMapInvalidateBuffer then GL_MAP_INVALIDATE_BUFFER_BIT else 0
    bfle  = if _bufferObjectMapFlagsMapFlushExplicit then GL_MAP_FLUSH_EXPLICIT_BIT else 0
    bmun  = if _bufferObjectMapFlagsMapUnsynchronized then GL_MAP_UNSYNCHRONIZED_BIT else 0

genBufferObject :: MonadIO m => BufferObjectSize -> BufferObjectAttribFlags -> m BufferObject
genBufferObject size attrib@BufferObjectAttribFlags {..} = do
  bufo@(BufferObject n) <- genObjectName
  glNamedBufferStorage n (_bufferObjectSizeObjectSize size) nullPtr bitF
  return bufo
  where
    bitF = marshallBufferObjectAttribFlags attrib

initBufferObject :: MonadIO m
                 => BufferObjectSize
                 -> BufferObjectAttribFlags
                 -> Ptr ()
                 -> m BufferObject
initBufferObject size attrib@BufferObjectAttribFlags {..} ptr = do
  bufo@(BufferObject n) <- genObjectName
  glNamedBufferStorage n (_bufferObjectSizeObjectSize size) ptr bitF
  return bufo
  where
    bitF = marshallBufferObjectAttribFlags attrib

bufferSubData :: MonadIO m => BufferObject -> BufferObjectSize -> BufferObjectOffset -> Ptr () -> m ()
bufferSubData (BufferObject n) (BufferObjectSize size) (BufferObjectOffset m) = glNamedBufferSubData n m size

{-
withNamedMappedBuffer :: (MonadIO m, MonadMask m) => BufferObject -> GL.BufferAccess -> (Ptr () -> m b) -> (GL.MappingFailure -> m b) -> m b
withNamedMappedBuffer t a action err
  = mapNamedBuffer t a
  >>= \case
        Nothing -> err GL.MappingFailed
        Just buf -> do (ret, ok) <- action buf `finallyRet` unmapNamedBuffer t
                       if ok
                         then return ret
                         else err GL.UnmappingFailed
-}

mapBuffer :: MonadIO m => BufferObject -> BufferAccess -> m (Maybe (Ptr ()))
mapBuffer t = fmap (maybeNullPtr Nothing Just) . mapBuffer_ t

mapBuffer_ :: MonadIO m => BufferObject -> BufferAccess -> m (Ptr ())
mapBuffer_ (BufferObject n) = glMapNamedBuffer n . marshallBufferAccess

unmapBuffer :: MonadIO m => BufferObject -> m Bool
unmapBuffer (BufferObject n) = fmap unmarshallGLboolean . glUnmapNamedBuffer $ n

mapBufferRange :: MonadIO m => BufferObject -> BufferObjectOffset -> BufferObjectSize -> BufferObjectMapFlags -> m (Ptr ())
mapBufferRange (BufferObject obj) offset leng flags
  = glMapNamedBufferRange obj (_bufferObjectOffsetObjectOffset offset) (_bufferObjectSizeObjectSize leng) (marshallBufferObjectMapFlags flags)

clearBufferSubData :: MonadIO m => BufferObject -> SizedFormat -> BufferObjectOffset -> BufferObjectSize -> SizedFormat -> GLDataType -> Ptr () -> m ()
clearBufferSubData (BufferObject obj) internalForm offset size form typ
  = glClearNamedBufferSubData obj (marshallSizedFormat internalForm) (_bufferObjectOffsetObjectOffset offset) (_bufferObjectSizeObjectSize size) (marshallSizedFormat form) (marshallGLDataType typ)

copyBufferSubData :: MonadIO m => BufferObject -> BufferObject -> BufferObjectOffset -> BufferObjectOffset -> BufferObjectSize -> m ()
copyBufferSubData (BufferObject readB) (BufferObject writeB) readOff writeOff size
  = glCopyNamedBufferSubData readB writeB (_bufferObjectOffsetObjectOffset readOff) (_bufferObjectOffsetObjectOffset writeOff) (_bufferObjectSizeObjectSize size)

bindVertexArrayObject :: MonadIO m => VertexArrayObject -> m ()
bindVertexArrayObject (VertexArrayObject n) = glBindVertexArray n

vertexArrayAttribBinding :: MonadIO m => VertexArrayObject -> AttribLocation -> AttribLocation -> m ()
vertexArrayAttribBinding (VertexArrayObject n) (AttribLocation attribindex) (AttribLocation bindindex)
  = glVertexArrayAttribBinding n attribindex bindindex

vertexArrayVertexBuffer :: MonadIO m => VertexArrayObject -> AttribLocation -> BufferObject -> BufferObjectOffset -> BufferObjectStride -> m ()
vertexArrayVertexBuffer (VertexArrayObject n) (AttribLocation bindindx) (BufferObject bufobj) offset stride
  = glVertexArrayVertexBuffer n bindindx bufobj (_bufferObjectOffsetObjectOffset offset) (_bufferObjectStrideObjectStride stride)

vertexArrayAttribFormat :: MonadIO m => VertexArrayObject -> AttribLocation -> BufferObjectComponentSize -> GLDataType -> IntegerHandling -> BufferObjectRelOffset -> m ()
vertexArrayAttribFormat (VertexArrayObject vaobj) (AttribLocation attribindx) size typ integerHandling relOffset
  = glVertexArrayAttribFormat vaobj attribindx (_bufferObjectComponentSizeObjectComponentSize size) (marshallGLDataType typ) handleFlag (_bufferObjectRelOffsetObjectRelOffset relOffset)
  where
    handleFlag = case integerHandling of
      Normalized -> GL_TRUE
      NotNormalized -> GL_FALSE

vertexArrayAttribEnable :: MonadIO m => VertexArrayObject -> AttribLocation -> m ()
vertexArrayAttribEnable (VertexArrayObject n) (AttribLocation loc) = glEnableVertexArrayAttrib n loc

vertexArrayAttribDisable :: MonadIO m => VertexArrayObject -> AttribLocation -> m ()
vertexArrayAttribDisable (VertexArrayObject n) (AttribLocation loc) = glDisableVertexArrayAttrib n loc

vertexArrayElementBuffer :: MonadIO m => VertexArrayObject -> BufferObject -> m ()
vertexArrayElementBuffer (VertexArrayObject n) (BufferObject m) = glVertexArrayElementBuffer n m

{-
textureSub1D :: TextureObject -> Pixel1DAttrib -> Int -> GL.SettableStateVar (Ptr ())
textureSub1D (TextureObject tobj) Pixel1DAttrib {..} level = GL.makeSettableStateVar $
  \ptr -> glTextureSubImage1D tobj lev xo width pixForm datType ptr
  where
    xo = maybe 0 fromIntegral _pixel1DAttribPixelXOffset
    lev = fromIntegral level
    pixForm = marshallPixelFormat _pixel1DAttribPixelFormat
    datType = marshallDataType _pixel1DAttribPixelType
    width  = fromIntegral _pixel1DAttribPixelWidth

textureSub2D :: TextureObject -> Pixel2DAttrib -> Int -> GL.SettableStateVar (Ptr ())
textureSub2D (TextureObject tobj) Pixel2DAttrib {..} level = GL.makeSettableStateVar $
  \ptr -> glTextureSubImage2D tobj lev xo yo width height pixForm datType ptr
  where
    xo = maybe 0 fromIntegral _pixel2DAttribPixelXOffset
    yo = maybe 0 fromIntegral _pixel2DAttribPixelYOffset
    lev = fromIntegral level
    pixForm = marshallPixelFormat _pixel2DAttribPixelFormat
    datType = marshallDataType _pixel2DAttribPixelType
    width  = fromIntegral _pixel2DAttribPixelWidth
    height = fromIntegral _pixel2DAttribPixelHeight

textureSub3D :: TextureObject -> Pixel3DAttrib -> Int -> GL.SettableStateVar (Ptr ())
textureSub3D (TextureObject tobj) Pixel3DAttrib {..} level = GL.makeSettableStateVar $
  \ptr -> glTextureSubImage3D tobj lev xo yo zo width height depth pixForm datType ptr
  where
    xo = maybe 0 fromIntegral _pixel3DAttribPixelXOffset
    yo = maybe 0 fromIntegral _pixel3DAttribPixelYOffset
    zo = maybe 0 fromIntegral _pixel3DAttribPixelZOffset
    lev = fromIntegral level
    pixForm = marshallPixelFormat _pixel3DAttribPixelFormat
    datType = marshallDataType _pixel3DAttribPixelType
    width  = fromIntegral _pixel3DAttribPixelWidth
    height = fromIntegral _pixel3DAttribPixelHeight
    depth  = fromIntegral _pixel3DAttribPixelDepth

textureBinding :: TextureTarget t => t -> GL.SettableStateVar TextureObject
textureBinding targ = GL.makeSettableStateVar $ \(TextureObject tobj)-> glBindTexture (marshallTextureTarget targ) tobj
-}

bindTextureUnit :: MonadIO m => TextureUnit -> (TextureObject t) -> m ()
bindTextureUnit (TextureUnit n) (TextureObject m) = glBindTextureUnit n m

toBufferObjectOffset :: Integral a => a -> BufferObjectOffset
toBufferObjectOffset = BufferObjectOffset . fromIntegral

toBufferObjectSize :: Integral a => a -> BufferObjectSize
toBufferObjectSize = BufferObjectSize . fromIntegral

toBufferObjectStride :: Integral a => a -> BufferObjectStride
toBufferObjectStride = BufferObjectStride . fromIntegral

marshallTextureParameter :: TextureParameter -> GLenum
marshallTextureParameter = \case
  TextureMinFilter -> GL_TEXTURE_MIN_FILTER
  TextureMagFilter -> GL_TEXTURE_MAG_FILTER
  TextureWrapS -> GL_TEXTURE_WRAP_S
  TextureWrapT -> GL_TEXTURE_WRAP_T
  TextureWrapR -> GL_TEXTURE_WRAP_R
  TextureBorderColor -> GL_TEXTURE_BORDER_COLOR
  TextureMinLOD -> GL_TEXTURE_MIN_LOD
  TextureMaxLOD -> GL_TEXTURE_MAX_LOD
  TextureBaseLevel -> GL_TEXTURE_BASE_LEVEL
  TextureMaxLevel -> GL_TEXTURE_MAX_LEVEL
  TextureCompareMode -> GL_TEXTURE_COMPARE_MODE
  TextureCompareFunc -> GL_TEXTURE_COMPARE_FUNC
  TextureLODBias -> GL_TEXTURE_LOD_BIAS

textureParameterf :: MonadIO m => TextureObject t -> TextureParameter -> GLfloat -> m ()
textureParameterf (TextureObject tobj) param =
  glTextureParameterf tobj (marshallTextureParameter param)

textureParameteri :: MonadIO m => TextureObject t -> TextureParameter -> GLint -> m ()
textureParameteri (TextureObject tobj) param =
  glTextureParameteri tobj (marshallTextureParameter param)

compileShader :: (Shader t, MonadIO m) => t -> m (Maybe ByteString)
compileShader t = do
  glCompileShader n
  status <- unmarshallGLboolean <$> foreignPoke (glGetShaderiv n GL_COMPILE_STATUS)
  if status
    then return Nothing
    else Just <$> withForeignBufferBS (glGetShaderiv n GL_INFO_LOG_LENGTH) (glGetShaderInfoLog n)
  where
    n = marshallShaderObject t

shaderDeleteStatus :: (Shader t, MonadIO m) => t -> m Bool
shaderDeleteStatus shader = unmarshallGLboolean <$> foreignPoke (glGetShaderiv (marshallShaderObject shader) GL_DELETE_STATUS)

shaderInfoLog :: (Shader t, MonadIO m) => t -> m ByteString
shaderInfoLog shader = withForeignBufferBS (glGetShaderiv n GL_INFO_LOG_LENGTH) (glGetShaderInfoLog n)
  where
    n = marshallShaderObject shader

shaderSource :: Shader t => t -> StateVar ByteString
shaderSource shader = makeStateVar getSource setSource
    where
      n = marshallShaderObject shader
      getSource = withForeignBufferBS (glGetShaderiv n GL_SHADER_SOURCE_LENGTH) (glGetShaderSource n)
      setSource src =
        withByteString src $ \srcPtr srcLength ->
        with srcPtr $ \srcPtrBuf ->
                        with srcLength $ \srcLengthBuf ->
                                           glShaderSource n 1 srcPtrBuf srcLengthBuf

programDeleteStatus :: MonadIO m => Program -> m Bool
programDeleteStatus (Program n) = unmarshallGLboolean <$> foreignPoke (glGetProgramiv n GL_DELETE_STATUS)

attachShader :: (MonadIO m, Shader t) => Program -> t -> m ()
attachShader (Program n) t = glAttachShader n (marshallShaderObject t)

linkProgram :: MonadIO m => Program -> m (Maybe ByteString)
linkProgram (Program n) = liftIO $ do
  glLinkProgram n
  status <- unmarshallGLboolean <$> foreignPoke (glGetProgramiv n GL_LINK_STATUS)
  if status
    then return Nothing
    else Just <$> withForeignBufferBS (glGetProgramiv n GL_INFO_LOG_LENGTH) (glGetProgramInfoLog n)

validateProgram :: MonadIO m => Program -> m (Maybe ByteString)
validateProgram (Program n) = liftIO $ do
  glValidateProgram n
  status <- unmarshallGLboolean <$> foreignPoke (glGetProgramiv n GL_VALIDATE_STATUS)
  if status
    then return Nothing
    else Just <$> withForeignBufferBS (glGetProgramiv n GL_INFO_LOG_LENGTH) (glGetProgramInfoLog n)

useProgram :: MonadIO m => Program -> m ()
useProgram (Program n) = glUseProgram n

{- Have to import GLU.Errors
getGLErrors :: MonadIO m => m ()
getGLErrors = liftIO G.errors >>= mapM_ print
-}
