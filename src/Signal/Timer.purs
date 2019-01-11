module Signal.Timer where

import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Signal (Signal(..), SignalConstructor)

foreign import timer :: forall r. Int -> Signal r Effect Number

foreign import intervalImpl :: forall r m a. Fn2 (SignalConstructor r m a) Int (Signal r m a)

interval :: forall r m. Int -> Signal r m Number
interval = runFn2 intervalImpl Signal
