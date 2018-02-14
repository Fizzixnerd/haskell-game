{-# LANGUAGE NoImplicitPrelude #-}

module Game.Entity.Player where

import ClassyPrelude
import Game.Types
import qualified Physics.Bullet as P
import qualified Linear as L
import Foreign.C.Types
import Control.Lens

allocatePlayerTransform :: MonadIO m => Player -> m (P.Transform)
allocatePlayerTransform p = liftIO $
                            P.getGhostObject (p ^. playerPhysicsController) >>=
                            P.pairCachingGhostObjectToCollisionObject >>=
                            P.coAllocateWorldTransform

withPlayerTransform :: MonadIO m => Player -> (P.Transform -> IO b) -> m b
withPlayerTransform p f = liftIO $ bracket (allocatePlayerTransform p) P.del f

getPlayerPosition :: MonadIO m => Player -> m (L.V3 CFloat)
getPlayerPosition p = withPlayerTransform p (\t -> do
                                                (x, y, z) <- P.getOrigin t
                                                return $ L.V3 x y z)

getPlayerOpenGLMatrix :: MonadIO m => Player -> m (L.M44 CFloat)
getPlayerOpenGLMatrix p = withPlayerTransform p P.getOpenGLMatrix

getPlayerLinearVelocity :: MonadIO m => Player -> m (L.V3 CFloat)
getPlayerLinearVelocity p = liftIO $ do
  (x, y, z) <- P.getLinearVelocity $ p ^. playerPhysicsController
  return $ L.V3 x y z

setPlayerLinearVelocity :: MonadIO m => Player -> L.V3 CFloat -> m ()
setPlayerLinearVelocity p v = liftIO $ P.setLinearVelocity 
                              (p ^. playerPhysicsController) 
                              (v ^. L._x) 
                              (v ^. L._y)
                              (v ^. L._z)

getPlayerAngularVelocity :: MonadIO m => Player -> m (L.V3 CFloat)
getPlayerAngularVelocity p = liftIO $ do
  (alpha, beta, gamma) <- P.getAngularVelocity $ p ^. playerPhysicsController
  return $ L.V3 alpha beta gamma

setPlayerAngularVelocity :: MonadIO m => Player -> L.V3 CFloat -> m ()
setPlayerAngularVelocity p omega = liftIO $ P.setAngularVelocity 
                                   (p ^. playerPhysicsController) 
                                   (omega ^. L._x) 
                                   (omega ^. L._y)
                                   (omega ^. L._z)
