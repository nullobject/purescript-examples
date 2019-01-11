module Main where

import Effect (Effect)
import Effect.Console (logShow)
import Effect.Random (random)
import Prelude (Unit, (=<<))

main :: Effect Unit
main = logShow =<< random
