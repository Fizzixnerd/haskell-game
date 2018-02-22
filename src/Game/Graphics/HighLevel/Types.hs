{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Game.Graphics.HighLevel.Types where

import Graphics.Binding
import ClassyPrelude
import qualified Data.Vector.Storable as VS

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
  uniform :: MonadIO m => a -> b -> UniformVariableContents a -> m ()

primsetting :: (ProgramLike a, PrimUniform b, MonadIO m) => UniformLocation -> a -> b -> m ()
primsetting = flip primMarshal

primsettingArray :: (ProgramLike a, PrimUniform b, MonadIO m) => UniformLocation -> a -> VS.Vector b -> m ()
primsettingArray = flip primMarshalArray

{-
class HasUniformBlock a b where
  bindBlock :: MSetter b a

class UniformBlockLike a where
  void glUniformBlockBinding( GLuint program​, GLuint uniformBlockIndex​, GLuint uniformBlockBinding​ );
-}
