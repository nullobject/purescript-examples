module Example8 where

import Prelude

import Control.Monad.Trans.Class (lift)
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Ref as Ref
import Signal (Signal, runSignal)
import Signal.Timer (interval)

scan :: forall a b. (b -> a -> b) -> b -> Signal a -> Signal b
scan f x s = do
  current <- lift $ Ref.new x
  a <- s
  b <- lift $ Ref.read current
  let r = f b a
  lift $ Ref.write r current
  pure r

main :: Effect Unit
main = do
  let s = interval 1000
      t = scan (+) 1.0 s

  runSignal t logShow
