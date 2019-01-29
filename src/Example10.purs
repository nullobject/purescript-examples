module Example10 where

import Prelude (Unit)

import Effect (Effect)
import Effect.Console (logShow)
import Signal (const, runSignal, zip)
import Signal.Timer (interval)

main :: Effect Unit
main = do
  let s = interval 1000
      t = const 2
      u = zip s t

  runSignal u logShow
