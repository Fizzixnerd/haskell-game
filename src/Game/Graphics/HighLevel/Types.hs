{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Game.Graphics.HighLevel.Types where

import Graphics.Binding
import ClassyPrelude
import Control.Lens
import Control.Lens.Internal.Setter (taintedDot, untaintedDot)
import qualified Data.Vector.Storable as VS
import Control.Lens.Action

type ForSetter s a = forall m. MonadIO m => Setter s (m s) () a
type ForGetter s a = forall m. MonadIO m => Action m s a
type ForOptical p q f s a = forall m. MonadIO m => Optical p q f s (m s) () a
type ForIndexPreservingSetter s a = forall m. MonadIO m => IndexPreservingSetter s (m s) () a

forSets :: (Profunctor p, Profunctor q, Settable f) => (p () a -> q s (IO s)) -> ForOptical p q f s a
forSets f = taintedDot . rmap liftIO . f . untaintedDot
{-# INLINE forSets #-}

forSetting :: (s -> a -> IO ()) -> ForIndexPreservingSetter s a
forSetting f = setting $ \g s -> liftIO (f s (g ())) >> return s
{-# INLINE forSetting #-}

forTo :: (s -> IO a) -> ForGetter s a
forTo f = act (liftIO . f)
{-# INLINE forTo #-}

stateSet :: ForIndexPreservingSetter (StateVar a) a
stateSet = forSetting $ \(StateVar _ set_) a -> set_ a

stateGet :: ForGetter (StateVar a) a
stateGet = act $ \(StateVar get_ _) -> liftIO get_

class GeneratableObjectName a => ProgramLike a where
  programDeleteStatus :: MonadIO m => a -> m Bool
  attachShader :: (MonadIO m, Shader t) => a -> t -> m ()
  linkProgram :: MonadIO m => a -> m (Maybe ByteString)
  validateProgram :: MonadIO m => a -> m (Maybe ByteString)
  useProgram :: MonadIO m => a -> m ()
  primMarshal :: (MonadIO m, PrimUniform b) => a -> UniformLocation -> b -> m ()
  primMarshalArray :: (MonadIO m, PrimUniform b) => a -> UniformLocation -> VS.Vector b -> m ()

instance ProgramLike Program where
  programDeleteStatus = programDeleteStatus'
  attachShader = attachShader'
  linkProgram = linkProgram'
  validateProgram = validateProgram'
  useProgram = useProgram'
  primMarshal = primMarshal_
  primMarshalArray = primMarshalArray_
-- Also need:
-- Block binding
class HasUniformVariable a b where
  type UniformVariableContents a
  uniform :: a -> ForSetter b (UniformVariableContents a)

primsetting :: (ProgramLike a, PrimUniform b) => UniformLocation -> ForSetter a b
primsetting loc = forSetting $ \prg x -> primMarshal prg loc x

primsettingArray :: (ProgramLike a, PrimUniform b) => UniformLocation -> ForSetter a (VS.Vector b)
primsettingArray loc = forSetting $ \prg x -> primMarshalArray prg loc x

{-
class HasUniformBlock a b where
  bindBlock :: MSetter b a

class UniformBlockLike a where
  void glUniformBlockBinding( GLuint program​, GLuint uniformBlockIndex​, GLuint uniformBlockBinding​ );
-}
