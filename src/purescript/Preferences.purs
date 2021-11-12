module Preferences where

import Prelude

import Control.Monad.State (get, put)
import Data.Generic.Rep (class Generic)
import Data.Array (zip)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (hush)
import Data.Enum (class Enum, upFromIncluding)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Maybe (Maybe, fromMaybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import OCES.Chess (Piece)
import OCES.Keyboard (Keycode)
import OCES.Preferences.Components.KeycodeInputField as KeycodeInputField
import OCSE.KeyboardControl.Keymap (Keymap, decodeEnumFunction, loadKeymap)
import Type.Proxy (Proxy(..))


newtype LabeledPiece = LabeledPiece Piece
instance KeycodeInputField.HtmlLabel LabeledPiece where
  htmlLabel (LabeledPiece p) = HH.text $ show p
derive instance genericLabeledPiece :: Generic LabeledPiece _
derive instance eqLabeledPiece :: Eq LabeledPiece
derive instance ordLabeledPiece :: Ord LabeledPiece
instance showLabeledPiece :: Show LabeledPiece where
  show = genericShow
instance boundedLabeledPiece :: Bounded LabeledPiece where
  top = genericTop
  bottom = genericBottom
instance boundedEnumLabeledPiece :: Enum LabeledPiece where
  succ = genericSucc
  pred = genericPred

type Slots = 
  ( keycodeInputField :: H.Slot KeycodeInputField.Query Void LabeledPiece
  )
_keycodeInputField = Proxy :: Proxy "keycodeInputField"

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
      mkPieceInput piece = HH.slot_ _keycodeInputField (LabeledPiece piece) KeycodeInputField.inputField 
        { value: LabeledPiece piece, keycode: state.pieceKey piece }
      piecesInputs = mkPieceInput <$> allEnumValues
      submitButton = HH.button [ HE.onClick \_ -> Save ] [ HH.text "Save" ]
   in HH.div [] (piecesInputs <> [submitButton])

initialState :: Input -> State
initialState keymap = keymap

handleAction :: forall m output. MonadEffect m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = 
  case _ of
  Save -> do
    keymap <- get
    let allPieces = allEnumValues
    piecesKeys <- traverse (\piece -> H.request _keycodeInputField piece KeycodeInputField.GetNewKeyCode) allPieces
    let pieceKey = fromMaybe keymap.pieceKey <<< decodePieceKey $ piecesKeys
    _ <- traverse (\piece -> H.tell _keycodeInputField piece KeycodeInputField.Commit) allPieces   
    put $ keymap { pieceKey = pieceKey }

decodePieceKey :: Array (Maybe Keycode) -> Maybe (Piece -> Keycode)
decodePieceKey piecesKeys = let 
      allPieces = allEnumValues :: Array Piece
      toEncodedKey (Tuple f s) = {key: show f, value: s}
  in do
  sequence piecesKeys >>= \pk -> hush <<< decodeEnumFunction $ toEncodedKey <$> zip allPieces pk

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
