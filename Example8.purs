module Example8 where

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Trans
import Data.Function
import Data.Tuple
import Debug.Trace

type Delay = Number

-- Represents a signal.
--
-- The type parameter `r` is the return type of the monad `m`. The callback
-- function takes a parameter `a`.
newtype Signal r m a = Signal ((a -> m r) -> m r)

type SignalConstructor r m a = (((a -> m r) -> m r) -> Signal r m a)

instance functorSignal :: (Monad m) => Functor (Signal r m) where
  (<$>) f s = Signal (\k -> runSignal s (\a -> k $ f a))

instance applySignal :: (Functor m, Monad m) => Apply (Signal r m) where
  (<*>) s t = Signal (\k -> runSignal s $ (\f -> runSignal t (\a -> (k $ f a))))

instance applicativeSignal :: (Functor m, Monad m) => Applicative (Signal r m) where
  pure a = Signal (\k -> k a)

instance bindSignal :: (Monad m) => Bind (Signal r m) where
  (>>=) m k = Signal (\k' -> runSignal m (\a -> runSignal (k a) k'))

instance monadSignal :: (Monad m) => Monad (Signal r m)

instance monadTransSignal :: MonadTrans (Signal r) where
  lift m = Signal (\k -> m >>= k)

type WithRef eff = Eff (ref :: Ref | eff)
type WithTimer eff = Eff (timer :: Timer | eff)

type StatefulSignal r eff a = Signal r (WithRef eff) a
type TimerSignal r eff a = Signal r (WithTimer eff) a

foreign import data Timer :: !

foreign import timerImpl
  "function timerImpl(signal, delay) {\
  \  return function(fn) {\
  \    return signal(function() {\
  \      setInterval(function() { fn(Math.random())(); }, delay);\
  \    });\
  \  };\
  \}" :: forall r m a. Fn2 (SignalConstructor r m a) Delay (Signal r m a)

timer :: forall r eff. Delay -> TimerSignal r eff Number
timer = runFn2 timerImpl Signal

runSignal :: forall r m a. (Monad m) => Signal r m a -> (a -> m r) -> m r
runSignal (Signal sf) f = sf f

callCC :: forall r m a b. (Monad m) => ((a -> Signal r m b) -> Signal r m a) -> Signal r m a
callCC f = Signal (\k -> runSignal (f (\a -> Signal (\_ -> k a))) k)

scan :: forall r eff a b. (b -> a -> b) -> b -> StatefulSignal r eff a -> StatefulSignal r eff b
scan f x s = do
  current <- lift $ newRef x
  callCC $ \k -> do
    a <- s
    b <- lift $ readRef current
    let r = f b a
    lift $ writeRef current r
    k r

main :: Eff (ref :: Ref, timer :: Timer, trace :: Trace) Unit
main = do
  let s = timer 100
      t = scan (+) 1 s

  runSignal t $ \n -> do
    print n
