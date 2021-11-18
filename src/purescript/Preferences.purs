module Preferences where

import Prelude

import Control.Monad.State (get, put)
import Data.Array (elem, filter, groupAll, null, zip)
import Data.Array.NonEmpty (head, length)
import Data.Either (hush)
import Data.Enum (class Enum, upFromIncluding)
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
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

      mkInputsGroup title children = HH.fieldset [] $ 
                                     [ HH.legend [] [ HH.text title ] ] <> children

      piecesInputs = mkPieceInput <$> allEnumValues
      piecesInputsGroup = mkInputsGroup "Pieces" piecesInputs

      disambiguationInputs = mkDisambiguationInput <$> allEnumValues
      disambiguationInputsGroup = mkInputsGroup "Disambiguation direction" disambiguationInputs

      cancelInputGroup = mkInputsGroup "Other keys" [ cancelInput ]

      submitButton = HH.button 
        [ HE.onClick \_ -> Save 
        , HP.classes [ ClassName "submit" ]
        ]
        [ HH.text "Save" ]
      gridClass = ClassName "grid"
      rowClass = ClassName "row"
      columnClass = ClassName "column"
   in HH.div 
        [ HP.classes [ gridClass ] ]
        [ HH.div 
          [ HP.classes [ rowClass ] ]
          [ HH.div 
            [ HP.classes [ columnClass ] ]
            [ piecesInputsGroup ]
          , HH.div 
            [ HP.classes [ columnClass ] ]
            [ disambiguationInputsGroup, cancelInputGroup ]
          ]
        , HH.div 
          [ HP.classes [ rowClass ] ]
          [ HH.div 
            [ HP.classes [ columnClass ] ]
            [ submitButton ]
          ]
        ]

initialState :: Input -> State
initialState keymap = keymap

handleAction :: forall m output. MonadEffect m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = 
  let
      duplicates :: forall x. Ord x => Array x -> Array x
      duplicates xs = groupAll xs # filter (\group -> length group > 1) <#> head
  in case _ of
  Save -> 
    let 
        commit newKeymap = do
          -- commit input fields values
          void $ traverse (\value -> H.tell _pieceKeycodeInputField value KeycodeInputField.Commit) allEnumValues
          void $ traverse (\value -> H.tell _disambiguaionKeycodeInputField value KeycodeInputField.Commit) allEnumValues
          H.tell _cancelKeycodeInputField LabeledCancel KeycodeInputField.Commit

          put newKeymap
          liftEffect <<< HA.runHalogenAff $ saveKeymap newKeymap

        alert pieces disambiguations shouldAlertOnCancelKey = do
          let errorMsg = "keycode used somewhere else"
          when shouldAlertOnCancelKey do
            H.tell _cancelKeycodeInputField LabeledCancel (KeycodeInputField.ShowError errorMsg)
          void $ traverse (\value -> H.tell _pieceKeycodeInputField (LabeledPiece value) (KeycodeInputField.ShowError errorMsg)) pieces
          void $ traverse (\value -> H.tell _disambiguaionKeycodeInputField (LabeledDisambiguationDirection value) (KeycodeInputField.ShowError errorMsg)) disambiguations
    in do
    keymap <- get

    -- get input field values to a function
    pieceKeyFun <- 
      traverse (\value -> H.request _pieceKeycodeInputField value KeycodeInputField.GetNewKeyCode) allEnumValues 
      <#> fromMaybe keymap.pieceKey <<< decodeMaybeKeys
    disambiguationKeyFun <- 
      traverse (\value -> H.request _disambiguaionKeycodeInputField value KeycodeInputField.GetNewKeyCode) allEnumValues 
      <#> fromMaybe keymap.disambiguationKey <<< decodeMaybeKeys
    cancelKey <- 
      H.request _cancelKeycodeInputField LabeledCancel KeycodeInputField.GetNewKeyCode
      <#> fromMaybe keymap.cancelKey
    let piecesKeys = pieceKeyFun <$> allEnumValues
        uniqueKeysGroupWithPieces = piecesKeys <> [cancelKey]
        disambiguationsKeys = disambiguationKeyFun <$> allEnumValues
        uniqueKeysGroupWithDisambiguations = disambiguationsKeys <> [cancelKey]
        pieceDuplicates = duplicates uniqueKeysGroupWithPieces
        disambiguationDuplicates = duplicates uniqueKeysGroupWithDisambiguations

        piecesToAlert = filter (\piece -> pieceKeyFun piece `elem` pieceDuplicates) allEnumValues
        disambiguationsToAlert = filter (\disambiguation -> disambiguationKeyFun disambiguation `elem` disambiguationDuplicates) allEnumValues
        shouldAlertOnCancelKey = cancelKey `elem` pieceDuplicates || cancelKey `elem` disambiguationDuplicates
        shouldAlert = shouldAlertOnCancelKey || not null piecesToAlert || not null disambiguationsToAlert

    if shouldAlert 
    then alert piecesToAlert disambiguationsToAlert shouldAlertOnCancelKey
    else do
      let 
        newKeymap = keymap 
          { pieceKey = pieceKeyFun
          , disambiguationKey = disambiguationKeyFun
          , cancelKey = cancelKey
          }
      commit newKeymap

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