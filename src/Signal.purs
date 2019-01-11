module Signal where

import Control.Monad.Trans.Class (class MonadTrans)
import Effect (Effect)
import Prelude

---- Represents a signal.
----
---- The type parameter `r` is the return type of the monad `m`. The callback
---- function takes a parameter `a`.
newtype SignalT r m a = SignalT (Callback r m a -> m r)

type Callback r m a = (a -> m r)
type Signal a = SignalT Unit Effect a
type SignalConstructor a = (((a -> Effect Unit) -> Effect Unit) -> Signal a)

instance functorSignal :: (Monad m) => Functor (SignalT r m) where
  map f s = SignalT (\k -> runSignal s (\a -> k $ f a))

instance applySignal :: (Functor m, Monad m) => Apply (SignalT r m) where
  apply s t = SignalT (\k -> runSignal s $ (\f -> runSignal t (\a -> (k $ f a))))

instance applicativeSignal :: (Functor m, Monad m) => Applicative (SignalT r m) where
  pure a = SignalT (\k -> k a)

instance bindSignal :: (Monad m) => Bind (SignalT r m) where
  bind m k = SignalT (\k' -> runSignal m (\a -> runSignal (k a) k'))

instance monadSignal :: (Monad m) => Monad (SignalT r m)

instance monadTransSignal :: MonadTrans (SignalT r) where
  lift m = SignalT (\k -> m >>= k)

-- Creates a new signal from a signal function.
mkSignal :: forall a. SignalConstructor a
mkSignal = SignalT

-- Runs a signal with a given callback function.
runSignal :: forall r m a. (Monad m) => SignalT r m a -> Callback r m a -> m r
runSignal (SignalT sf) f = sf f
