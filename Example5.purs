module Example5 where

import Control.Monad.Eff
import Debug.Trace

foreign import data Random :: !

foreign import random
  "function random(f) {\
  \  return f(Math.random());\
  \}" :: forall eff a.
         (Number -> Eff eff a) ->
         Eff (random :: Random | eff) Unit

foreign import data Timer   :: !
foreign import data Timeout :: *

foreign import timeout
  "function timeout(time) {\
  \  return function(fn) {\
  \    return function() {\
  \      return setTimeout(fn, time);\
  \    };\
  \  };\
  \}" :: forall a eff.
          Number ->
          (Eff (timer :: Timer | eff) a) ->
          Eff (timer :: Timer | eff) Timeout

main = do
  timeout 100 $ do
    random print
    main
