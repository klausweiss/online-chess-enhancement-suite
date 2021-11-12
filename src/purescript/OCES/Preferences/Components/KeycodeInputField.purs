module OCES.Preferences.Components.KeycodeInputField where

import Prelude

import Control.Monad.State (get, modify_, put)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Unfoldable as Unfoldable
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import OCES.Keyboard (Keycode)
import OCES.Keyboard as Keyboard

class HtmlLabel v where
  htmlLabel :: forall w i. v -> HH.HTML w i

type Input v = 
  { value :: v
  , keycode :: Keycode 
  }

type State v = 
  { value :: v
  , committedKeycode :: Keycode
  , newKeycode :: Maybe Keycode
  }

data Query a
  = GetNewKeyCode (Keycode -> a)
  | Commit a

data Action
  = SetNewKeycode (Maybe Keycode)

inputField :: forall v output m. HtmlLabel v => H.Component Query (Input v) output m
inputField =
  H.mkComponent 
    { initialState
    , render
    , eval: H.mkEval H.defaultEval 
      { handleQuery = handleQuery
      , handleAction = handleAction
      }
    }

initialState :: forall v. Input v -> State v
initialState { value, keycode } = 
  { value
  , committedKeycode: keycode
  , newKeycode: Nothing
  }

render :: forall m v. HtmlLabel v => State v -> H.ComponentHTML Action () m 
render { value, committedKeycode, newKeycode } =
  let options 
        = (\keycode -> HH.option 
            [ HP.value (show keycode) 
            , HP.selected (keycode == Maybe.fromMaybe committedKeycode newKeycode)
            ]
            [ HH.text (Keyboard.toHumanReadable keycode) ])
        <$> Keyboard.supportedKeycodes
      select = HH.select 
                [ HE.onValueChange (parseKeycode >>> SetNewKeycode) ]
                options
      maybeOldValue = Unfoldable.fromMaybe 
        $ const committedKeycode <$> newKeycode
        <#> \kc -> HH.span 
          [ HP.classes [ ClassName "old-value" ] ] 
          [ HH.text $ " was " <> Keyboard.toHumanReadable kc <> " before"]
   in HH.div [] $ [ htmlLabel value, select ] <> maybeOldValue

parseKeycode :: String -> Maybe Keycode
parseKeycode = Int.fromString

handleQuery :: forall v output a m. Query a -> H.HalogenM (State v) Action () output m (Maybe a)
handleQuery = 
  let getKeycode = do
        { committedKeycode, newKeycode } <- get
        pure $ Maybe.fromMaybe committedKeycode newKeycode
  in case _ of
  GetNewKeyCode reply -> do
    keycode <- getKeycode
    pure <<< Just <<< reply $ keycode
  Commit a -> do
    keycode <- getKeycode
    modify_ (\s -> s { committedKeycode = keycode, newKeycode = Nothing })
    pure $ Just a

handleAction :: forall v m output. Action -> H.HalogenM (State v) Action () output m Unit
handleAction = 
  case _ of
  SetNewKeycode Nothing -> do
    pure unit
  SetNewKeycode (Just keycode) -> do
    state <- get
    put $ state { newKeycode = Just keycode } 
