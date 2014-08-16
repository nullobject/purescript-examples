module Example3 where

import Control.Bind
import Control.Monad
import Control.Monad.Eff.Random
import Data.Tuple

import Debug.Trace

-- Represents a signal function.
data SF a b =
    SFConst b       -- | A stateless (constant) signal function.
  | SFPure (a -> b) -- | A pure signal function

-- Steps a given signal function with an input value and returns a tuple
-- containing the output value and a new signal function.
step :: forall m a b. (Monad m) => SF a b -> a -> m (Tuple b (SF a b))
step sf@(SFConst c) input = return $ Tuple c sf
step sf@(SFPure f) input = return $ Tuple (f input) sf

sfconst = SFConst 1

sfpure = SFPure (\x -> x + 1)

stepSF :: forall m a b. (Monad m) => SF a b -> a -> m b
stepSF sf input  = do
  output <- step sf input
  return $ fst output

main = do
  output <- stepSF sfpure 0
  print output

  u <- step sfconst 0
  print $ fst u
