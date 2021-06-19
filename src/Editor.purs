module Editor where

import Prelude

import Web.UIEvent.InputEvent (InputEvent)
import Effect (Effect)

foreign import insertDiv :: String -> String -> Effect Unit
foreign import getValue :: InputEvent -> { id :: String, content :: String }
