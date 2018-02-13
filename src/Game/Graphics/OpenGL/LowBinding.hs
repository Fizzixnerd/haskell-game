{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Game.Graphics.OpenGL.LowBinding
  ( module X
  , genBufferObject
  , initBufferObject
  , namedBufferSubData
--  , withNamedMappedBuffer
  , mapNamedBuffer
  , unmapNamedBuffer
  , clearNamedBufferSubData
  , copyNamedBufferSubData
  , vertexArrayAttribBinding
  , vertexArrayVertexBuffer
  , vertexArrayAttribFormat
  , vertexArrayAttribEnable
  , mapNamedBufferRange
  , textureSub1D
  , textureSub2D
  , textureSub3D
  , bindTextureUnit
  , toBufferObjectOffset
  , toBufferObjectSize
  , toBufferObjectStride
  , vertexArrayElementBuffer
  , textureBinding
  , textureParameterf
  , textureParameteri
  , genVAO
  ) where

import Graphics.Rendering.OpenGL.GL as X
  ( VertexArrayObject
  , BufferObject
  , AttribLocation(..)
  , DataType
  , SettableStateVar
  , Capability(..)
  , MappingFailure(..)
  , UniformLocation(..)
  , VariableType(..)
  , Uniform(..)
  , uniform
  , uniformv
  , Shader
  , ShaderType(..)
  , createShader
  , shaderSourceBS
  , compileShader
  , shaderInfoLog
  , Program
  , createProgram
  , currentProgram
  , attachShader
  , linkProgram
  , validateProgram
  , programInfoLog
  , ($=)
  , clear
  , ClearBuffer(..)
  , TextureUnit(..)
  , drawElements
  , bindVertexArrayObject
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
  )

import Data.ObjectName as X

import Linear.OpenGL ()
import ClassyPrelude
import Foreign.Ptr
import Foreign.Marshal.Array
import Game.Graphics.OpenGL.LowTypes
import Graphics.GL.Core45
import Graphics.GL.Types
import qualified Graphics.Rendering.OpenGL.GL as GL
import Data.Bits ((.|.))
import Unsafe.Coerce
import qualified Control.Exception.Safe as CE

--- Be careful!
bufferObjectToGLuint :: GL.BufferObject -> GLuint
bufferObjectToGLuint = unsafeCoerce

gluintToBufferObject :: GLuint -> GL.BufferObject
gluintToBufferObject = unsafeCoerce

vertexArrayObjectToGLuint :: GL.VertexArrayObject -> GLuint
vertexArrayObjectToGLuint = unsafeCoerce

gluintToVertexArrayObject :: GLuint -> GL.VertexArrayObject
gluintToVertexArrayObject = unsafeCoerce
---

marshallDataType :: GLDataType -> GLenum
marshallDataType = \case
   GLUnsignedByte -> GL_UNSIGNED_BYTE
   GLByte -> GL_BYTE
   GLUnsignedShort -> GL_UNSIGNED_SHORT
   GLShort -> GL_SHORT
   GLUnsignedInt -> GL_UNSIGNED_INT
   GLInt -> GL_INT
   GLHalfFloat -> GL_HALF_FLOAT
   GLFloat -> GL_FLOAT
   GLDouble -> GL_DOUBLE
   GLUnsignedByte332 -> GL_UNSIGNED_BYTE_3_3_2
   GLUnsignedByte233Rev -> GL_UNSIGNED_BYTE_2_3_3_REV
   GLUnsignedShort565 -> GL_UNSIGNED_SHORT_5_6_5
   GLUnsignedShort565Rev -> GL_UNSIGNED_SHORT_5_6_5_REV
   GLUnsignedShort4444 -> GL_UNSIGNED_SHORT_4_4_4_4
   GLUnsignedShort4444Rev -> GL_UNSIGNED_SHORT_4_4_4_4_REV
   GLUnsignedShort5551 -> GL_UNSIGNED_SHORT_5_5_5_1
   GLUnsignedShort1555Rev -> GL_UNSIGNED_SHORT_1_5_5_5_REV
   GLUnsignedInt8888 -> GL_UNSIGNED_INT_8_8_8_8
   GLUnsignedInt8888Rev -> GL_UNSIGNED_INT_8_8_8_8_REV
   GLUnsignedInt1010102 -> GL_UNSIGNED_INT_10_10_10_2
   GLUnsignedInt2101010Rev -> GL_UNSIGNED_INT_2_10_10_10_REV
   GLUnsignedInt248 -> GL_UNSIGNED_INT_24_8
   GLUnsignedInt10f11f11fRev -> GL_UNSIGNED_INT_10F_11F_11F_REV
   GLUnsignedInt5999Rev -> GL_UNSIGNED_INT_5_9_9_9_REV
   GLFloat32UnsignedInt248Rev -> GL_FLOAT_32_UNSIGNED_INT_24_8_REV

{-
marshallGLboolean :: Bool -> GLboolean
marshallGLboolean x = if x then GL_TRUE else GL_FALSE
-}

unmarshallGLboolean :: (Eq a, Num a) => a -> Bool
unmarshallGLboolean = (/= GL_FALSE)

marshallBufferAccess :: GL.BufferAccess -> GLenum
marshallBufferAccess = \case
   GL.ReadOnly -> GL_READ_ONLY
   GL.WriteOnly -> GL_WRITE_ONLY
   GL.ReadWrite -> GL_READ_WRITE

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


genBufferObject :: MonadIO m => BufferObjectSize -> BufferObjectAttribFlags -> m GL.BufferObject
genBufferObject size attrib@BufferObjectAttribFlags {..} = do
  bufo <- GL.genObjectName
  glNamedBufferStorage (bufferObjectToGLuint bufo) (_bufferObjectSizeObjectSize size) nullPtr bitF
  return bufo
  where
    bitF = marshallBufferObjectAttribFlags attrib

initBufferObject :: MonadIO m
                 => BufferObjectSize
                 -> BufferObjectAttribFlags
                 -> Ptr ()
                 -> m GL.BufferObject
initBufferObject size attrib@BufferObjectAttribFlags {..} ptr = do
  bufo <- liftIO . allocaArray 1 $ \buff -> do
    glCreateBuffers 1 buff
    bufo <- unsafeHead <$> peekArray 1 buff
    return $ gluintToBufferObject bufo

  glNamedBufferStorage (bufferObjectToGLuint bufo) (_bufferObjectSizeObjectSize size) ptr bitF

  return bufo
  where
    bitF = marshallBufferObjectAttribFlags attrib

namedBufferSubData :: GL.BufferObject -> BufferObjectSize -> GL.SettableStateVar (BufferObjectOffset, Ptr ())
namedBufferSubData bufo bufsize = GL.makeSettableStateVar $
  \(offset, dat) -> glNamedBufferSubData (bufferObjectToGLuint bufo) (_bufferObjectOffsetObjectOffset offset) (_bufferObjectSizeObjectSize bufsize) dat

{-
withNamedMappedBuffer :: (MonadIO m, MonadMask m) => GL.BufferObject -> GL.BufferAccess -> (Ptr () -> m b) -> (GL.MappingFailure -> m b) -> m b
withNamedMappedBuffer t a action err
  = mapNamedBuffer t a
  >>= \case
        Nothing -> err GL.MappingFailed
        Just buf -> do (ret, ok) <- action buf `finallyRet` unmapNamedBuffer t
                       if ok
                         then return ret
                         else err GL.UnmappingFailed
-}

mapNamedBuffer :: MonadIO m => GL.BufferObject -> GL.BufferAccess -> m (Maybe (Ptr ()))
mapNamedBuffer t = fmap (maybeNullPtr Nothing Just) . mapNamedBuffer_ t

mapNamedBuffer_ :: MonadIO m => GL.BufferObject -> GL.BufferAccess -> m (Ptr ())
mapNamedBuffer_ t = glMapNamedBuffer (bufferObjectToGLuint t) . marshallBufferAccess

unmapNamedBuffer :: MonadIO m => GL.BufferObject -> m Bool
unmapNamedBuffer = fmap unmarshallGLboolean . glUnmapNamedBuffer . bufferObjectToGLuint

mapNamedBufferRange :: MonadIO m => GL.BufferObject -> BufferObjectOffset -> BufferObjectSize -> BufferObjectMapFlags -> m (Ptr ())
mapNamedBufferRange obj offset leng flags
  = glMapNamedBufferRange (bufferObjectToGLuint obj) (_bufferObjectOffsetObjectOffset offset) (_bufferObjectSizeObjectSize leng) (marshallBufferObjectMapFlags flags)

clearNamedBufferSubData :: MonadIO m => GL.BufferObject -> SizedFormat -> BufferObjectOffset -> BufferObjectSize -> SizedFormat -> GLDataType -> Ptr () -> m ()
clearNamedBufferSubData obj internalForm offset size form typ
  = glClearNamedBufferSubData (bufferObjectToGLuint obj) (marshallSizedFormat internalForm) (_bufferObjectOffsetObjectOffset offset) (_bufferObjectSizeObjectSize size) (marshallSizedFormat form) (marshallDataType typ)

copyNamedBufferSubData :: MonadIO m => GL.BufferObject -> GL.BufferObject -> BufferObjectOffset -> BufferObjectOffset -> BufferObjectSize -> m ()
copyNamedBufferSubData readB writeB readOff writeOff size
  = glCopyNamedBufferSubData (bufferObjectToGLuint readB) (bufferObjectToGLuint writeB) (_bufferObjectOffsetObjectOffset readOff) (_bufferObjectOffsetObjectOffset writeOff) (_bufferObjectSizeObjectSize size)

vertexArrayAttribBinding :: MonadIO m => GL.VertexArrayObject -> GL.AttribLocation -> GL.AttribLocation -> m ()
vertexArrayAttribBinding vaobj (GL.AttribLocation attribindex) (GL.AttribLocation bindindex)
  = glVertexArrayAttribBinding (vertexArrayObjectToGLuint vaobj) attribindex bindindex

vertexArrayVertexBuffer :: MonadIO m => GL.VertexArrayObject -> GL.AttribLocation -> GL.BufferObject -> BufferObjectOffset -> BufferObjectStride -> m ()
vertexArrayVertexBuffer vaobj (GL.AttribLocation bindindx) bufobj offset stride
  = glVertexArrayVertexBuffer (vertexArrayObjectToGLuint vaobj) bindindx (bufferObjectToGLuint bufobj) (_bufferObjectOffsetObjectOffset offset) (_bufferObjectStrideObjectStride stride)

vertexArrayAttribFormat :: MonadIO m => GL.VertexArrayObject -> GL.AttribLocation -> BufferObjectComponentSize -> GLDataType -> IntegerHandling -> BufferObjectRelOffset -> m ()
vertexArrayAttribFormat vaobj (GL.AttribLocation attribindx) size typ integerHandling relOffset
  = glVertexArrayAttribFormat (vertexArrayObjectToGLuint vaobj ) attribindx (_bufferObjectComponentSizeObjectComponentSize size) (marshallDataType typ) handleFlag (_bufferObjectRelOffsetObjectRelOffset relOffset)
  where
    handleFlag = case integerHandling of
      Normalized -> GL_TRUE
      NotNormalized -> GL_FALSE

vertexArrayAttribEnable :: GL.VertexArrayObject -> GL.AttribLocation -> GL.SettableStateVar GL.Capability
vertexArrayAttribEnable vaobj (GL.AttribLocation loc) = GL.makeSettableStateVar $
  \case
    GL.Enabled -> glEnableVertexArrayAttrib (vertexArrayObjectToGLuint vaobj) loc
    GL.Disabled -> glDisableVertexArrayAttrib (vertexArrayObjectToGLuint vaobj) loc

vertexArrayElementBuffer :: GL.VertexArrayObject -> GL.SettableStateVar GL.BufferObject
vertexArrayElementBuffer vao = GL.makeSettableStateVar $ glVertexArrayElementBuffer (vertexArrayObjectToGLuint vao) . bufferObjectToGLuint

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

bindTextureUnit :: GL.TextureUnit -> GL.SettableStateVar TextureObject
bindTextureUnit (GL.TextureUnit n) = GL.makeSettableStateVar $
  \(TextureObject texObj) -> glBindTextureUnit n texObj

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

textureParameterf :: MonadIO m => TextureObject -> TextureParameter -> GLfloat -> m ()
textureParameterf (TextureObject tobj) param =
  glTextureParameterf tobj (marshallTextureParameter param)
textureParameteri :: MonadIO m => TextureObject -> TextureParameter -> GLint -> m ()
textureParameteri (TextureObject tobj) param =
  glTextureParameteri tobj (marshallTextureParameter param)

genVAO :: MonadIO m => m GL.VertexArrayObject
genVAO = liftIO . allocaArray 1 $ \buff -> do
  glCreateVertexArrays 1 buff
  vobj <- unsafeHead <$> peekArray 1 buff
  return $ gluintToVertexArrayObject vobj

{- Have to import GLU.Errors
getGLErrors :: MonadIO m => m ()
getGLErrors = liftIO G.errors >>= mapM_ print
-}
