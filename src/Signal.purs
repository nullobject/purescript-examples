module Signal where

import Control.Monad.Trans.Class (class MonadTrans)
import Effect (Effect)
import Prelude

-- | Represents a signal.
-- |
-- | The type parameter `r` is the return type of the monad `m`. The callback
-- | function takes a parameter `a`.
-- |
-- | This is based on the continuation monad.
newtype SignalT a m r = SignalT ((r -> m a) -> m a)

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

-- Runs a signal with a given callback function.
runSignal :: forall a m r. (Monad m) => SignalT a m r -> (r -> m a) -> m a
runSignal (SignalT sf) f = sf f

callCC :: forall a b. ((a -> Signal b) -> Signal a) -> Signal a
callCC f = SignalT (\k -> runSignal (f (\a -> SignalT (\_ -> k a))) k)
