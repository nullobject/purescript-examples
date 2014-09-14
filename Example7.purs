module Example7 where

import Control.Monad.Eff
import Control.Monad.State
import Control.Monad.State.Class
import Data.Tuple
import Debug.Trace

incState :: State Number Unit
incState = modify $ (*) 2

testState :: State Number String
testState = do
  incState
  incState
  return "Done"

main :: Eff (trace :: Trace) Unit
main = do
  case runState testState 1 of
    Tuple value state -> do
      print $ "state: " ++ (show state)
      print $ "value: " ++ (show value)
