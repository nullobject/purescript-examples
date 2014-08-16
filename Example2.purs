module Example2 where

import Control.Bind
import Control.Monad.Eff.Random

import Debug.Trace

main = print =<< random
