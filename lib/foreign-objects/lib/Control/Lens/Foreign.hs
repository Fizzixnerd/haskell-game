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

class (MonadIO m, Functor f) => Alien f m where
  immigrate :: m (f (m a)) -> f (m a)

-- type MonLens m s t a b = forall f. Alien f m => (a -> f (m b)) -> s -> f (m t)

instance Monad m => Alien Identity m where
  immigrate mIma = Identity . join $ runIdentity <$> mIma
  {-# INLINE immigrate #-}

instance Monad m => Alien (Const (m r)) m where
  immigrate mCmr = Const . join $ getConst <$> mCmr
  {-# INLINE immigrate #-}

type MonGetting m r s a = (a -> Const (m r) (m a)) -> s -> Const (m r) (m s)
type MonGetter m s a = MonGetting m a s a
type MonOptical m p q f s a = Optical p q f s (m s) a (m a)
type MonSetting m p q f s a = Optical p q f s (m s) () (m a)
type MonSetter m s a = Setter s (m s) () (m a)

type MonLens m s t a b = forall f. Alien f m => (a -> f b) -> s -> f t

monView :: Applicative m => MonGetting m a s a -> s -> m a
monView l = getConst #. l (Const . pure)
{-# INLINE monView #-}

-- p a (m a) ->
monTo :: Monad m => (s -> m a) -> MonGetting m a s a
monTo sma f s = pure s <$ res
  where
    res = immigrate (f <$> sma s)

infixr 8 ^!
(^!) :: Monad m => s -> MonGetting m a s a -> m a
s ^! l = getConst (l (Const . pure) s)
{-# INLINE (^!) #-}

-- Chain setters using >>= after the first &
type ForSetter s a = forall m. MonadIO m => Setter s (m s) () (m a)
type ForGetter s a = forall m. MonadIO m => MonGetting m a s a
type ForLens   s a = forall f m. (Alien f m, MonadIO m) => (a -> f a) -> s -> f s

type ForIndexPreservingSetter s a = forall m. MonadIO m => IndexPreservingSetter s (m s) () (m a)

monSets :: (Profunctor p, Profunctor q, Settable f) => (p () (m a) -> q s (m s)) -> MonSetting m p q f s a
monSets f = taintedDot .  f . untaintedDot
{-# INLINE monSets #-}

forSetting :: (s -> a -> IO ()) -> ForIndexPreservingSetter s a
forSetting f = setting $ \g s -> (g () >>= (liftIO . f s)) >> return s
{-# INLINE forSetting #-}


forTo :: (s -> IO a) -> ForGetter s a
forTo f = monTo (liftIO . f)
{-# INLINE forTo #-}

monLens :: Alien f m => (s -> IO a) -> (s -> a -> IO s) -> (a -> f a) -> (s -> f s)
monLens sia sais afma s = res
  where
    ma = liftIO $ sia s
    fma = immigrate (afma <$> ma)
    ams = liftIO . sais s
    res = (ams =<<) <$> fma

stateSet :: ForIndexPreservingSetter (StateVar a) a
stateSet = forSetting $ \(StateVar _ set_) a -> set_ a

{-
stateGet :: ForGetter (StateVar a) a
stateGet = act $ \(StateVar get_ _) -> liftIO get_
-}
{-
infixr 4 .~!
infixl 8 ^.!
(.~!) :: MonadIO m => StateVar a -> a -> m (StateVar a)
(^.!) :: MonadIO m => StateVar a -> m a
-}
