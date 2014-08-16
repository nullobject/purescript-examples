module Example4 where

import Control.Bind
import Control.Monad
import Control.Monad.Eff.Random
import Data.Tuple

import Debug.Trace

-- Represents a signal.
data Signal a = Signal a

instance showSignal :: (Show a) => Show (Signal a) where
  show (Signal x) = "Signal " ++ show x

-- Returns a constant signal.
constant :: forall a. a -> Signal a
constant x = Signal x

-- Returns a given function lifted into a signal function.
lift :: forall a b. (a -> b) -> Signal a -> Signal b
lift f (Signal x) = Signal (f x)

inc x = x + 1

main = do
  print $ (lift inc $ constant 1)
