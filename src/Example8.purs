module Example8 where

import Prelude

import Control.Monad.Trans.Class (lift)
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Ref as Ref
import Signal (Signal, mkSignal, runSignal)
import Signal.Timer (interval)

callCC :: forall a b. ((a -> Signal b) -> Signal a) -> Signal a
callCC f = mkSignal (\k -> runSignal (f (\a -> mkSignal (\_ -> k a))) k)

scan :: forall a b. (b -> a -> b) -> b -> Signal a -> Signal b
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
