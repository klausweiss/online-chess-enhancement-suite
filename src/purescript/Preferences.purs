module Preferences where

import Prelude

import Control.Monad.State (get, put)
import Data.Array (zip)
import Data.Either (hush)
import Data.Enum (class Enum, upFromIncluding)
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import OCES.Keyboard (Keycode)
import OCES.Preferences.Components.CancelKeycodeInputField (LabeledCancel(..))
import OCES.Preferences.Components.DisambiguationKeycodeInputField (LabeledDisambiguationDirection(..))
import OCES.Preferences.Components.KeycodeInputField as KeycodeInputField
import OCES.Preferences.Components.PieceKeycodeInputField (LabeledPiece(..))
import OCSE.KeyboardControl.Keymap (Keymap, decodeEnumFunction, loadKeymap, saveKeymap)
import Type.Proxy (Proxy(..))


type Slots = 
  ( pieceKeycodeInputField :: H.Slot KeycodeInputField.Query Void LabeledPiece
  , disambiguaionKeycodeInputField :: H.Slot KeycodeInputField.Query Void LabeledDisambiguationDirection
  , cancelKeycodeInputField :: H.Slot KeycodeInputField.Query Void LabeledCancel
  )
_pieceKeycodeInputField = Proxy :: Proxy "pieceKeycodeInputField"
_disambiguaionKeycodeInputField  = Proxy :: Proxy "disambiguaionKeycodeInputField"
_cancelKeycodeInputField = Proxy :: Proxy "cancelKeycodeInputField"

type Input = Keymap

type State = Keymap

data Action
  = Save

data Message = Updated Int

allEnumValues :: forall e. Enum e => Bounded e => Array e
allEnumValues = upFromIncluding bottom
render :: forall m. State -> HH.ComponentHTML Action Slots m
render state = 
  let
      mkPieceInput piece 
        = HH.slot_ _pieceKeycodeInputField (LabeledPiece piece) KeycodeInputField.inputField 
        { value: LabeledPiece piece, keycode: state.pieceKey piece }

      mkDisambiguationInput disambiguationDirection 
        = HH.slot_ _disambiguaionKeycodeInputField (LabeledDisambiguationDirection disambiguationDirection) KeycodeInputField.inputField 
        { value: LabeledDisambiguationDirection disambiguationDirection, keycode: state.disambiguationKey disambiguationDirection }

      cancelInput 
        = HH.slot_ _cancelKeycodeInputField LabeledCancel KeycodeInputField.inputField 
        { value: LabeledCancel, keycode: state.cancelKey }

      piecesInputs = mkPieceInput <$> allEnumValues
      disambiguationInputs = mkDisambiguationInput <$> allEnumValues
      submitButton = HH.button [ HE.onClick \_ -> Save ] [ HH.text "Save" ]
   in HH.div [] (piecesInputs <> disambiguationInputs <> [cancelInput] <> [submitButton])

initialState :: Input -> State
initialState keymap = keymap

handleAction :: forall m output. MonadEffect m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = 
  case _ of
  Save -> do
    keymap <- get

    -- get input field values to a function
    pieceKey <- 
      traverse (\value -> H.request _pieceKeycodeInputField value KeycodeInputField.GetNewKeyCode) allEnumValues 
      <#> fromMaybe keymap.pieceKey <<< decodeMaybeKeys
    disambiguationKey <- 
      traverse (\value -> H.request _disambiguaionKeycodeInputField value KeycodeInputField.GetNewKeyCode) allEnumValues 
      <#> fromMaybe keymap.disambiguationKey <<< decodeMaybeKeys
    cancelKey <- 
      H.request _cancelKeycodeInputField LabeledCancel KeycodeInputField.GetNewKeyCode
      <#> fromMaybe keymap.cancelKey

    -- commit input fields values
    void $ traverse (\value -> H.tell _pieceKeycodeInputField value KeycodeInputField.Commit) allEnumValues
    void $ traverse (\value -> H.tell _disambiguaionKeycodeInputField value KeycodeInputField.Commit) allEnumValues
    H.tell _cancelKeycodeInputField LabeledCancel KeycodeInputField.Commit

    let newKeymap = keymap 
          { pieceKey = pieceKey
          , disambiguationKey = disambiguationKey 
          , cancelKey = cancelKey
          }
    put newKeymap
    liftEffect <<< HA.runHalogenAff $ saveKeymap newKeymap

decodeMaybeKeys :: forall e. Show e => Bounded e => Enum e => Array (Maybe Keycode) -> Maybe (e -> Keycode)
decodeMaybeKeys maybeKeys = 
  let 
    allValues = allEnumValues :: Array e
    toEncodedKey (Tuple f s) = {key: show f, value: s}
  in do
  sequence maybeKeys >>= \keys -> hush <<< decodeEnumFunction $ toEncodedKey <$> zip allValues keys

component :: forall m query output. MonadEffect m => H.Component query Input output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  keymap <- loadKeymap
  runUI component keymap body
