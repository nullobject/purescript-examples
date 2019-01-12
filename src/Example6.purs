module Example6 where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Signal (runSignal)
import Signal.Timer (timer)

main :: Effect Unit
main = do
  let s = (_ *  1.0) <$> timer 50
      t = (_ * -1.0) <$> timer 50
      u = (+) <$> s <*> t

  runSignal u $ \n -> do
    logShow n
    main
