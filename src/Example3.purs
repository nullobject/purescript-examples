module Example3 where

import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Console (logShow)
import Prelude

-- Represents a signal function.
data SF a b =
    SFConst b       -- | A stateless (constant) signal function.
  | SFPure (a -> b) -- | A pure signal function

sfconst :: forall a. SF a Int
sfconst = SFConst 1

sfpure :: SF Int Int
sfpure = SFPure (\x -> x + 1)

-- Steps a given signal function with an input value and returns a tuple
-- containing the output value and a new signal function.
step :: forall m a b. (Monad m) => SF a b -> a -> m (Tuple b (SF a b))
step sf@(SFConst c) input = pure $ Tuple c sf
step sf@(SFPure f) input = pure $ Tuple (f input) sf

stepSF :: forall m a b. (Monad m) => SF a b -> a -> m b
stepSF sf input = do
  output <- step sf input
  pure $ fst output

main :: Effect Unit
main = do
  output <- stepSF sfpure 0
  logShow output

  u <- step sfconst 0
  logShow $ fst u
