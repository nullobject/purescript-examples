module Example5 where

import Control.Monad.Eff
import Debug.Trace

foreign import data Random :: !
foreign import data Timer  :: !

foreign import random
  "function random(f) {\
  \  return function() {\
  \    return f(Math.random())();\
  \  };\
  \}" :: forall a eff.
         (Number -> Eff eff a) ->
         Eff (random :: Random | eff) Unit

foreign import timeout
  "function timeout(time) {\
  \  return function(fn) {\
  \    return function() {\
  \      setTimeout(fn, time);\
  \    };\
  \  };\
  \}" :: forall a eff.
         Number ->
         Eff (timer :: Timer | eff) a ->
         Eff (timer :: Timer | eff) Unit

main = timeout 10 $ do
  random print
  main
