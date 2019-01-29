module Example9 where

import Prelude ((+), Unit, map)

import Effect (Effect)
import Effect.Console (logShow)
import Signal (const, runSignal)

main :: Effect Unit
main = do
  let s = const 5
      t = map (_ + 1) s

  runSignal t logShow
