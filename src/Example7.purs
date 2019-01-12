module Example7 where

import Prelude

import Control.Monad.State (State, runState)
import Control.Monad.State.Class (modify)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

incState :: State Int Unit
incState = void $ modify (_ * 2)

testState :: State Int String
testState = do
  incState
  incState
  pure "Done"

main :: Effect Unit
main = do
  case runState testState 1 of
    Tuple value state -> do
      log $ "state: " <> (show state)
      log $ "value: " <> (show value)
