module Data.Random where

import Effect (Effect)

foreign import random :: Effect Number
