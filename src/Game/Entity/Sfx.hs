{-# LANGUAGE NoImplicitPrelude #-}

module Game.Entity.Sfx where

import Game.Types
import qualified Linear as L
import qualified Sound.OpenAL as AL
import Game.Entity.WorldTransform
import Control.Lens
import ClassyPrelude
import Foreign.C.Types

playEntity :: Entity s -> Game s ()
playEntity e = case e ^. entitySounds of
  Nothing -> return ()
  Just sfx -> do
    (L.V3 x y z) <- getWorldPosition $ e ^. entityCollisionObject
    forM_ (sfx ^. sfxSources) $ \s -> do
      AL.sourcePosition s AL.$= AL.Vertex3 (CFloat x) (CFloat y) (CFloat z)
      sState <- liftIO $ AL.sourceState s
      case sState of
        AL.Initial -> AL.play [s]
        _ -> return ()
