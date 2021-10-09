module Signal.DOM.Prevented where

import Effect (Effect)
import Signal (Signal, constant)

foreign import keyPressedPWithPrevent :: forall c. (c -> Signal c) -> Boolean -> Int -> Effect (Signal Boolean)

keyPressed :: Boolean -> Int -> Effect (Signal Boolean)
keyPressed preventDefault = keyPressedPWithPrevent constant preventDefault
