{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Model.Loader where

import ClassyPrelude
import qualified Codec.Wavefront as W
import Foreign.C.Types
import Game.Types
import qualified Data.Vector.Storable as VS
import qualified Data.Map.Strict as MS
import qualified Linear as L
import Control.Lens
import qualified Control.Monad.State.Strict as MS

type ExpandObjVTNState = MS.State ExpandObjVTN

loadObj :: MonadIO m => FilePath -> m W.WavefrontOBJ
loadObj fp = do
  eObj :: Either String W.WavefrontOBJ <- W.fromFile fp
  case eObj of
    Left e -> error e
    Right obj -> return obj

tupleFaceIndex :: W.FaceIndex -> (Int, Int, Int)
tupleFaceIndex fi = fromMaybe (error "Face not in VTN format") mres
  where
    mres = (,,) (W.faceLocIndex fi - 1) <$> (subtract 1 <$> W.faceTexCoordIndex fi) <*> (subtract 1 <$> W.faceNorIndex fi)

objToVTNPoint :: W.Location -> W.TexCoord -> W.Normal -> VTNPoint
objToVTNPoint (W.Location x y z w) (W.TexCoord r s _) (W.Normal nx ny nz) = VTNPoint loc tex nor
  where
    loc = L.V4 (CFloat x) (CFloat y) (CFloat z) (CFloat w)
    tex = L.V2 (CFloat r) (CFloat (1-s))
    nor = L.V3 (CFloat nx) (CFloat ny) (CFloat nz)

fetchVTNPoint :: (Int, Int, Int) -> ExpandObjVTNState VTNPoint
fetchVTNPoint (v, t, n) = do
  mver <- preuse $ expandObjVTNVerts.ix v
  mtex <- preuse $ expandObjVTNTexs.ix t
  mnor <- preuse $ expandObjVTNNorms.ix n
  return . fromMaybe (error "Invalid VTN format: could not fetch point.") $ objToVTNPoint <$> mver <*> mtex <*> mnor

fetchVTNIndex :: VTNPoint -> VTNIndex -> ExpandObjVTNState CUInt
fetchVTNIndex vtnPt vtnIdx = join $ do
  nextIdx <- use expandObjVTNNextInd
  expandObjVTNIndMap.at vtnIdx %%= updating nextIdx
  where
    updating _ (Just x) = (return x, Just x)
    updating n Nothing  = (expandObjVTNNextInd += 1 >> expandObjVTNPoints %= (vtnPt:) >> return n, Just n)

updateVTNMap :: W.FaceIndex -> ExpandObjVTNState ()
updateVTNMap fi = do
  vtnPt <- fetchVTNPoint idx
  newIdx <- fetchVTNIndex vtnPt vtnIdx
  expandObjVTNIndices %= (newIdx:)
  where
    idx@(v, t, n) = tupleFaceIndex fi
    vtnIdx = VTNIndex (fromIntegral v) (fromIntegral t) (fromIntegral n)

addVTNFace :: W.Face -> ExpandObjVTNState ()
addVTNFace (W.Face a b c _) = updateVTNMap a >> updateVTNMap b >> updateVTNMap c

expandVTNObj :: W.WavefrontOBJ -> (VS.Vector VTNPoint, VS.Vector CUInt)
expandVTNObj obj = (expandedPoints, expandedIndices)
  where
    objVerts = W.objLocations obj
    objTexs  = W.objTexCoords obj
    objNorms = W.objNormals obj
    faces    = (\W.Element {..} -> elValue) <$> W.objFaces obj

    initState = ExpandObjVTN MS.empty [] [] 0 objVerts objTexs objNorms

    finState = MS.execState (mapM_ addVTNFace faces) initState
    expandedPoints = finState ^. expandObjVTNPoints . to (VS.fromList . reverse)
    expandedIndices = finState ^. expandObjVTNIndices . to (VS.fromList . reverse)

loadObjVTN :: MonadIO m => FilePath -> m (VS.Vector VTNPoint, VS.Vector CUInt)
loadObjVTN = fmap expandVTNObj . loadObj
