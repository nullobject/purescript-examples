module Effect.Timer where

import Effect (Effect)
import Prelude (Unit)

foreign import setTimeout :: Int -> Effect Unit -> Effect Unit
