module OCES.Preferences.Components.PieceKeycodeInputField where

import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Halogen (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import OCES.Chess (Piece)
import OCES.Preferences.Components.KeycodeInputField as KeycodeInputField


newtype LabeledPiece = LabeledPiece Piece
instance KeycodeInputField.HtmlLabel LabeledPiece where
  htmlLabel (LabeledPiece p) = 
    HH.div
      [ HP.classes [ ClassName "icon", ClassName "piece", ClassName (show p) ]
      , HP.title $ show p
      ] []

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
