module Signal.DOM.KeyEvent where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Signal (Signal, constant)

foreign import keyPressedImpl :: forall c. (c -> Signal c) -> Int -> Effect (Signal KeyEvent)
keyPressed :: forall eff. MonadEffect eff => Int -> eff (Signal KeyEvent)
keyPressed = liftEffect <<< keyPressedImpl constant

foreign import data KeyEvent :: Type
foreign import noopEvent :: KeyEvent

foreign import preventDefaultImpl :: KeyEvent -> Effect Unit
preventDefault :: forall eff. MonadEffect eff => KeyEvent -> eff Unit
preventDefault = liftEffect <<< preventDefaultImpl

foreign import isKeyDown :: KeyEvent -> Boolean
foreign import isKeyUp :: KeyEvent -> Boolean
foreign import keycode :: KeyEvent -> Int
