module Format where

import Prelude (Unit)

import Effect (Effect)

foreign import bold :: Effect Unit
foreign import italic :: Effect Unit
foreign import underline :: Effect Unit
foreign import mkList :: Effect Unit
foreign import mkHeading :: Int -> Effect Unit
