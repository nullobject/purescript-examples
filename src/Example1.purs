module Example1 where

import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit)

main :: Effect Unit
main = do
  log "Hello sailor!"
