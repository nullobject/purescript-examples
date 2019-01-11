module Example8 where

import Control.Monad.Trans.Class (lift)
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Ref as Ref
import Prelude
import Signal (Signal(..), runSignal)
import Signal.Timer (interval)

callCC :: forall r m a b. (Monad m) => ((a -> Signal r m b) -> Signal r m a) -> Signal r m a
callCC f = Signal (\k -> runSignal (f (\a -> Signal (\_ -> k a))) k)

scan :: forall r a b. (b -> a -> b) -> b -> Signal r Effect a -> Signal r Effect b
scan f x s = do
  current <- lift $ Ref.new x
  callCC $ \k -> do
    a <- s
    b <- lift $ Ref.read current
    let r = f b a
    lift $ Ref.write r current
    k r

main :: Effect Unit
main = do
  let s = interval 1000
      t = scan (+) 1.0 s

  runSignal t $ \n -> do
    logShow n
