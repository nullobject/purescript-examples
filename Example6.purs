module Example6 where

import Control.Monad.Eff
import Debug.Trace

type Delay = Number

-- Represents a signal.
--
-- The type parameter `r` is the return type of the monad `m`. The callback
-- function takes a parameter `a`.
newtype Signal r m a = Signal ((a -> m r) -> m r)

instance functorSignal :: (Monad m) => Functor (Signal r m) where
  (<$>) f s = Signal (\k -> runSignal s (\a -> k $ f a))

instance applySignal :: (Functor m, Monad m) => Apply (Signal r m) where
  (<*>) s t = Signal (\k -> runSignal s $ (\f -> runSignal t (\a -> (k $ f a))))

foreign import data Timer :: !

foreign import timer
  "function timer(t) {\
  \  return Signal.create(function(fn) {\
  \    return function() {\
  \      setTimeout(function() { fn(Math.random())(); }, t);\
  \    };\
  \  });\
  \}" :: forall r eff. Delay -> Signal r (Eff (timer :: Timer | eff)) Number

runSignal :: forall r m a. (Monad m) => Signal r m a -> (a -> m r) -> m r
runSignal (Signal sf) f = sf f

main :: Eff (timer :: Timer, trace :: Trace) Unit
main = do
  let s = ((*) ( 1)) <$> timer 50
      t = ((*) (-1)) <$> timer 50
      u = (+) <$> s <*> t

  runSignal u $ \n -> do
    print n
    main
