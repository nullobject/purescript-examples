module Example5 where

import Prelude

import Data.Random (random)
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Timer (setTimeout)

main :: Effect Unit
main = setTimeout 1000 $ do
  logShow =<< random
