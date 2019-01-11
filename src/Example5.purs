module Example5 where

import Data.Random (random)
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Timer (setTimeout)
import Prelude

main :: Effect Unit
main = setTimeout 1000 $ do
  logShow =<< random
