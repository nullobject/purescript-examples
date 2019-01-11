module Signal.Timer where

import Data.Function.Uncurried (Fn2, runFn2)
import Signal (Signal, SignalConstructor, mkSignal)

foreign import timer :: Int -> Signal Number

foreign import intervalImpl :: forall a. Fn2 (SignalConstructor a) Int (Signal a)

interval :: Int -> Signal Number
interval = runFn2 intervalImpl mkSignal
