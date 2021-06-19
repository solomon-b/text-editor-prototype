module Caret where

import Prelude (Unit)

import Effect (Effect)

foreign import getOffset :: Effect Int
foreign import setOffset :: Int -> Effect Unit
foreign import shiftOffset :: Int -> Effect Unit
