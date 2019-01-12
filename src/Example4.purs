module Example4 where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)

-- Represents a signal.
data Signal a = Signal a

instance showSignal :: (Show a) => Show (Signal a) where
  show (Signal x) = "Signal " <> show x

-- Returns a constant signal.
constant :: forall a. a -> Signal a
constant x = Signal x

-- Returns a given function lifted into a signal function.
lift :: forall a b. (a -> b) -> Signal a -> Signal b
lift f (Signal x) = Signal (f x)

inc :: Int -> Int
inc x = x + 1

main :: Effect Unit
main = do
  logShow $ (lift inc $ constant 1)
