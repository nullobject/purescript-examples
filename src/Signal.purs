module Signal where

import Control.Monad.Trans.Class (class MonadTrans)
import Effect (Effect)
import Prelude

---- Represents a signal.
----
---- The type parameter `r` is the return type of the monad `m`. The callback
---- function takes a parameter `a`.
newtype Signal r m a = Signal ((a -> m r) -> m r)

type SignalConstructor r m a = (((a -> m r) -> m r) -> Signal r m a)

instance functorSignal :: (Monad m) => Functor (Signal r m) where
  map f s = Signal (\k -> runSignal s (\a -> k $ f a))

instance applySignal :: (Functor m, Monad m) => Apply (Signal r m) where
  apply s t = Signal (\k -> runSignal s $ (\f -> runSignal t (\a -> (k $ f a))))

instance applicativeSignal :: (Functor m, Monad m) => Applicative (Signal r m) where
  pure a = Signal (\k -> k a)

instance bindSignal :: (Monad m) => Bind (Signal r m) where
  bind m k = Signal (\k' -> runSignal m (\a -> runSignal (k a) k'))

instance monadSignal :: (Monad m) => Monad (Signal r m)

instance monadTransSignal :: MonadTrans (Signal r) where
  lift m = Signal (\k -> m >>= k)

runSignal :: forall r m a. (Monad m) => Signal r m a -> (a -> m r) -> m r
runSignal (Signal sf) f = sf f
