{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Game.Graphics.Model.AssImp where

import ClassyPrelude
import Asset.AssImp.Types
import Asset.AssImp.Import
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc
import Game.Types

import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V (generateM)
import qualified Linear as L

type AssImpScene            = ScenePtr
type AssImpMesh             = MeshPtr
type AssImpVertexCoords     = Vector3DPtr
type AssImpTextureCoordsPtr = Ptr Vector3DPtr
type AssImpNormalCoords     = Vector3DPtr
type AssImpFaces            = FacePtr

importAssImpFileGood :: FilePath -> IO AssImpScene
importAssImpFileGood = flip importFile 0
--importAssImpFileGood = importAndProcessFileFast

data RawAssImpMesh = RawAssImpMesh
  { _rawAssImpMeshNumVertices     :: Word32
  , _rawAssImpMeshNumUVComponents :: Ptr Word32
  , _rawAssImpMeshVertexCoords    :: AssImpVertexCoords
  , _rawAssImpMeshTextureCoords   :: AssImpTextureCoordsPtr
  , _rawAssImpMeshNormalCoords    :: AssImpNormalCoords
  , _rawAssImpMeshFaces           :: (Ptr CUInt, Int)
  } deriving (Eq, Ord)

getRawAssImpMesh :: AssImpMesh -> IO RawAssImpMesh
getRawAssImpMesh ptr = do
  numV <- (\(CUInt x) -> x) <$> meshNumVertices ptr
  numUV <- castPtr <$> meshNumUVComponents ptr
  vptr <- meshVertices ptr
  nptr <- meshNormals ptr
  tptrptr <- meshTextureCoords ptr
  faces <- bufferFaces ptr
  return $ RawAssImpMesh numV numUV vptr tptrptr nptr faces

{-
data AssImpMesh2D = AssImpMesh2D
  { _rawAssImpMesh2DVertexCoords    :: !(VS.Vector (L.V3 Float))
  , _rawAssImpMesh2DTextureCoords   :: !(VS.Vector (L.V2 Float))
  , _rawAssImpMesh2DNormalCoords    :: !(VS.Vector (L.V3 Float))
  }
-}

getSceneMeshes :: AssImpScene -> IO (Vector RawAssImpMesh)
getSceneMeshes sc = do
  numMeshes <- fromIntegral <$> sceneNumMeshes sc
  mptr <- sceneMeshes sc
  V.generateM numMeshes $ peekElemOff mptr >=> getRawAssImpMesh

-- Can probably interleave here, if I want.
-- Also, perhaps we can assume that the texture dimension is constant
-- across a mesh?
getAssImpMesh2D :: RawAssImpMesh -> IO (VS.Vector AssImpVertex, VS.Vector Word32)
getAssImpMesh2D RawAssImpMesh {..} = do
  vfptr <- mallocForeignPtrArray vsize :: IO (ForeignPtr CFloat)
  withForeignPtr vfptr $ \destptr ->
    copyBytes destptr (castPtr _rawAssImpMeshVertexCoords) vsize
  let vvec = VS.unsafeFromForeignPtr0 (castForeignPtr vfptr) n

  VS.forM_ vvec $ print . show

  nfptr <- mallocForeignPtrArray nsize :: IO (ForeignPtr CFloat)
  withForeignPtr nfptr $ \destptr ->
    copyBytes destptr (castPtr _rawAssImpMeshNormalCoords) nsize
  let nvec = VS.unsafeFromForeignPtr0 (castForeignPtr nfptr) n

  tptr <- peek _rawAssImpMeshTextureCoords
  tvec <- VS.generateM n $ \i -> do
    let m = sizeOf (0 :: Float)
    x <- peekByteOff tptr $ 3*m*i   :: IO Float
    y <- peekByteOff tptr $ m*(3*i+1) :: IO Float
    return $ L.V2 x (1-y)

  let (fptr, fnum) = _rawAssImpMeshFaces
  ffptr <- newForeignPtr finalizerFree (castPtr fptr)
  let ivec = VS.unsafeFromForeignPtr0 ffptr fnum
  return (VS.zipWith3 AssImpVertex vvec tvec nvec, ivec)
  where
    n = fromIntegral _rawAssImpMeshNumVertices
    vsize = fromIntegral $ 3 * n * sizeOf (0 :: CFloat)
    nsize = fromIntegral $ 3 * n * sizeOf (0 :: CFloat)

loadAssImpMeshes2D :: FilePath -> IO (Vector (VS.Vector AssImpVertex, VS.Vector Word32))
loadAssImpMeshes2D =     importAssImpFileGood
                     >=> getSceneMeshes
                     >=> mapM getAssImpMesh2D
