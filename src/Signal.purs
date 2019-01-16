module Signal
  (
    Signal
  , SignalConstructor
  , SignalT(..)
  , const
  , runSignal
  , module Control.Monad.Cont.Class
  ) where

import Prelude

import Control.Monad.Cont.Class (class MonadCont, callCC)
import Control.Monad.Trans.Class (class MonadTrans)
import Effect (Effect)

-- | This signal monad transformer.
-- |
-- | This monad transform is based on the continuation monad.
-- |
-- | The type parameter `r` is the return type of the monad `m`. The callback
-- | function takes a parameter `a`.
newtype SignalT r m a = SignalT ((a -> m r) -> m r)

type Signal a = SignalT Unit Effect a
type SignalConstructor a = ((a -> Effect Unit) -> Effect Unit) -> Signal a

-- | Runs a signal with a given callback function.
runSignal :: forall r m a. (Monad m) => SignalT r m a -> (a -> m r) -> m r
runSignal (SignalT s) k = s k

-- | Creates a signal that emits a single value.
const :: forall r m a. (Monad m) => a -> SignalT r m a
const a = callCC \cont -> cont a

instance monadContSignalT :: Monad m => MonadCont (SignalT r m) where
  callCC f = SignalT (\k -> case f (\a -> SignalT (\_ -> k a)) of SignalT f' -> f' k)

instance functorSignal :: (Monad m) => Functor (SignalT r m) where
  map f (SignalT s) = SignalT (\k -> s (\a -> k $ f a))

instance applySignal :: (Functor m, Monad m) => Apply (SignalT r m) where
  apply (SignalT s) (SignalT t) = SignalT (\k -> s (\g -> t (\a -> k (g a))))

instance applicativeSignal :: (Functor m, Monad m) => Applicative (SignalT r m) where
  pure a = SignalT (\k -> k a)

instance bindSignal :: (Monad m) => Bind (SignalT r m) where
  bind (SignalT s) k = SignalT (\k' -> s (\a -> case k a of SignalT s' -> s' k'))

instance monadSignal :: (Monad m) => Monad (SignalT r m)

instance monadTransSignal :: MonadTrans (SignalT r) where
  lift m = SignalT (\k -> m >>= k)
