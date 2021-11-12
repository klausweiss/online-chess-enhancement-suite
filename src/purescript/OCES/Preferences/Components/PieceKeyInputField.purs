module OCES.Preferences.Components.PieceKeyInputField where

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
import OCES.Chess (Piece)
import OCES.Keyboard (Keycode)
import OCES.Keyboard as Keyboard

type Input = 
  { piece :: Piece 
  , keycode :: Keycode 
  }

type State = 
  { piece :: Piece
  , committedKeycode :: Keycode
  , newKeycode :: Maybe Keycode
  }

data Query a
  = GetNewKeyCode (Keycode -> a)
  | Commit a

data Action
  = SetNewKeycode (Maybe Keycode)

inputField :: forall output m. H.Component Query Input output m
inputField =
  H.mkComponent 
    { initialState
    , render
    , eval: H.mkEval H.defaultEval 
      { handleQuery = handleQuery
      , handleAction = handleAction
      }
    }

initialState :: Input -> State
initialState { piece, keycode } = 
  { piece
  , committedKeycode: keycode
  , newKeycode: Nothing
  }

render :: forall m. State -> H.ComponentHTML Action () m 
render { piece, committedKeycode, newKeycode } =
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
      label = HH.text (show piece <> " ")
      maybeOldValue = Unfoldable.fromMaybe 
        $ const committedKeycode <$> newKeycode
        <#> \kc -> HH.span 
          [ HP.classes [ ClassName "old-value" ] ] 
          [ HH.text $ " was " <> Keyboard.toHumanReadable kc <> " before"]
   in HH.div [] $ [ label, select ] <> maybeOldValue

parseKeycode :: String -> Maybe Keycode
parseKeycode = Int.fromString

handleQuery :: forall output a m. Query a -> H.HalogenM State Action () output m (Maybe a)
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

handleAction :: forall m output. Action -> H.HalogenM State Action () output m Unit
handleAction = 
  case _ of
  SetNewKeycode Nothing -> do
    pure unit
  SetNewKeycode (Just keycode) -> do
    state <- get
    put $ state { newKeycode = Just keycode } 
