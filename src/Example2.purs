module Example2 where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Effect.Random (random)

main :: Effect Unit
main = logShow =<< random
