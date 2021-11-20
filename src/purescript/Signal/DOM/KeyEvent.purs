module Signal.DOM.KeyEvent where

import Prelude

import Effect (Effect)
import Signal (Signal, constant)

foreign import keyPressedFFI :: forall c. (c -> Signal c) -> Int -> Effect (Signal KeyEvent)

foreign import data KeyEvent :: Type
foreign import noopEvent :: KeyEvent

foreign import preventDefault :: KeyEvent -> Effect Unit
foreign import isKeyDown :: KeyEvent -> Boolean
foreign import isKeyUp :: KeyEvent -> Boolean
foreign import keycode :: KeyEvent -> Int


keyPressed :: Int -> Effect (Signal KeyEvent)
keyPressed = keyPressedFFI constant
