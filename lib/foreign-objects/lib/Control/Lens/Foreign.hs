{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Control.Lens.Foreign where
{-
  ( module X
  , ForSetter
  , ForGetter
  , ForOptical
  , ForIndexPreservingSetter
  , forSets
  , forSetting
  , forTo
  , stateSet
  , stateGet
  ) where
-}
import Control.Lens
import Control.Lens.Internal.Setter (taintedDot, untaintedDot)
import Control.Monad.IO.Class
import Data.StateVar
import Data.Profunctor.Unsafe ((#.), (.#))
import Control.Monad

class Functor f => Alien f m where
  immigrate :: m (f (m a)) -> f (m a)

type MonLens m s t a b = forall f. Alien f m => (a -> f (m b)) -> s -> f (m t)

instance Monad m => Alien Identity m where
  immigrate mIma = Identity . join $ runIdentity <$> mIma

instance Monad m => Alien (Const (m r)) m where
  immigrate mCmr = Const . join $ getConst <$> mCmr

type ForLens s a = forall f m. (Alien f m, MonadIO m) => (a -> f (m a)) -> s -> f (m s)


forLens :: (s -> IO a) -> (s -> a -> IO s) -> ForLens s a
forLens sia sais afma s = res
  where
    ma = liftIO $ sia s
    fma = immigrate (afma <$> ma)
    ams = liftIO . sais s
    res = (ams =<<) <$> fma

type MonGetting m r s a = (a -> Const (m r) (m a)) -> s -> Const (m r) (m s)

-- type ForGetting r s a = forall t f m. (MonadIO m, Alien f m) => (a -> Const (m r) a) -> s -> Const (m r) (m s)


monView :: Monad m => MonGetting m a s a -> s -> m a
monView l = getConst #. l (Const . pure)
{-# INLINE monView #-}


infixr 8 ^!
(^!) :: Monad m => s -> MonGetting m a s a -> m a
s ^! l = getConst (l (Const . pure) s)
{-# INLINE (^!) #-}

{-
monLens :: Monad m => (s -> m a) -> (s -> a -> m s) -> MonLens m s s a a
monLens sma sams afma s = res
  where
    ma   = sma s
    ams  = sams s
    afms = fmap ams . afma
    mfms = afms <$> ma
    res = immigrate mfms
-}


{-
{-
-- Setter:
-- (a -> Identity (m b)) -> s -> Identity (m t)
-- Getting:
-- (a -> Const r a) -> s -> Const r s
-- Getter:
-- (a -> f a) -> s -> f s
-- Lens:
-- (a -> f b) -> s -> f t
-- (a -> (f m) b)) -> s -> (f m) t)

-- My Getting:
-- (a -> Const (m r) a) -> s -> Const (m r) s

-- Alien f b r => (a -> f b) -> s -> f t

{-
class Functor f => Alien f a r m where
  immigrate :: f a -> m r

class Resident f a r where
  naturalize :: f a -> IO r

instance Alien Identity (IO a) a IO where
  immigrate = liftIO . runIdentity
  {-# INLINE immigrate #-}

instance Alien (Const (IO r)) a r IO where
  immigrate = liftIO . getConst
  {-# INLINE immigrate #-}
-}
{-
class Alien f a r m where
  immigrate :: f a -> m r
  emigrate  :: m r -> f a

  immigrateDot :: Profunctor p => p b (f a) -> p b (m r)
  immigrateDot g = g `seq` rmap immigrate g
  {-# INLINE immigrateDot #-}

  emigrateDot :: Profunctor p => p b (m r) -> p b (f a)
  emigrateDot g = g `seq` rmap emigrate g
  {-# INLINE emigrateDot #-}

instance Alien Identity (m a) a m where
  immigrate = runIdentity
  {-# INLINE immigrate #-}
  emigrate  = Identity
  {-# INLINE emigrate #-}
  immigrateDot = (runIdentity #.)
  {-# INLINE immigrateDot #-}
  emigrateDot  = (Identity #.)
  {-# INLINE emigrateDot #-}
-}

{-
class Alien f a r m where
  immigrate :: m (f a) -> m r
  {-# INLINE immigrate #-}
  emigrate :: m r -> f a
  {-# INLINE emigrate #-}
-}
{-
instance Alien (Const (m r)) a r m where
  immigrate = getConst
  {-# INLINE immigrate #-}
  emigrate = Const
  {-# INLINE emigrate #-}
-}

--
-- * General types
type QuiteOptical p q r r' m m' f s t a b = (Alien f b r m, Alien f t r' m') => p a (f b) -> q s (f t)
type ForOptical p q r r' f s t a b = forall m. MonadIO m => QuiteOptical p q r r' m m f s t a b

-- * Setting
type ForSetter s t a b = forall f m. (Functor f, MonadIO m, Alien f b t m, Settable f) => (a -> f b) -> s -> f t

type ForIndexPreservingSetter s t a b = forall m f p. (MonadIO m, Alien f a t m, Conjoined p, Settable f) => p a (f b) -> p s (f t)

{-
forSetting :: (s -> a -> IO ()) -> ForIndexPreservingSetter s s a a
forSetting act_ pafa = untaintedDot pafa
  where
    act_' :: (a -> )
{-# INLINE forSetting #-}
-}

-- * Getting
type ForGetting r s a = forall m. MonadIO m => (a -> Const (IO r) a) -> s -> Const (m r) a

type ForGetter s a = forall f m. (MonadIO m, Alien f a a m, Contravariant f) => (a -> f a) -> s -> f s

-- * Lenses
-}

{-
class Within f a r m where
  internalize :: m r -> f a

class Without f a r m where
  externalize :: m (f a) -> m r
-}

class Alien f a r m where
{-
  immigrate :: MonadIO m => f a -> m r
  emigrate  :: MonadIO m => m r -> f a
-}

-- type ForLens s t a b = forall f m. (Functor f, MonadIO m, Alien f a a m) => (a -> f b) -> s -> f t

-- type ForLensLike m r s a = forall f. Alien f a r m => (a -> f a) -> (s -> f s)

-- type ForLens s a = forall f r m. (Functor f, Alien f a r m) => (a -> f a) -> s -> f s

type ForLensLike m s a = forall f r. Alien f a r m => (a -> f a) -> (s -> f s)

-- (a -> f (m a)) -> (s -> f (m s))
-- g :: s -> m a
-- h :: s -> a -> m s
-- forLens g h act s = res
--   where
forLens :: Monad m => (s -> m a) -> (s -> a -> m s) -> 

{-
forLens :: (s -> IO a) -> (s -> a -> IO ()) -> ForLens s a
forLens sia saio afa s = emigrate (liftIO is_)
  where
    ia  = sia s
    ifa = afa <$> ia
    aio = saio s

    is_ = (ia >>= aio) >> return s
-}
{-
forLens :: (s -> IO a) -> (s -> a -> IO ()) -> ForLens s a
forLens get_ set_ afa s = res
  where
    ma = immigrate $ afa <$> (liftIO $ get_ s)
    ms  = (liftIO . \a -> (set_ s a >> return s)) <$> ma
-}
{-
class Functor f => Alien r f | f -> r where
  naturalize   :: (Functor f, MonadIO m) => m (f a) -> f (m a)
  unnaturalize :: (Functor f, MonadIO m) => f (m a) -> m (f a)

  immigrate :: IO r -> f a
  emigrate  :: MonadIO m => f a -> m r
instance Alien r (Const r) where
  immigrate = Const #. runIdentity
  {-# INLINE immigrate #-}
  emigrate = Identity #. getConst
  {-# INLINE emigrate #-}
  -}

{-
-- Chain setters using >>= after the first &
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

{-
infixr 4 .~!
infixl 8 ^.!
(.~!) :: MonadIO m => StateVar a -> a -> m (StateVar a)
(^.!) :: MonadIO m => StateVar a -> m a
-}
-}
-}
