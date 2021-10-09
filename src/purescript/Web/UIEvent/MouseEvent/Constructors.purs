module Web.UIEvent.MouseEvent.Constructors where

import Web.Event.Event (EventType)
import Web.UIEvent.MouseEvent (MouseEvent)


foreign import makeMouseEvent 
  :: EventType 
  -> Int -- clientX
  -> Int -- clientY
  -> MouseEvent

