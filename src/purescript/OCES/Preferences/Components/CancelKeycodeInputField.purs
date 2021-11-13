module OCES.Preferences.Components.CancelKeycodeInputField where

import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Halogen.HTML as HH
import OCES.Preferences.Components.KeycodeInputField as KeycodeInputField


data LabeledCancel = LabeledCancel
instance KeycodeInputField.HtmlLabel LabeledCancel where
  htmlLabel LabeledCancel = HH.text $ "cancel"

derive instance genericLabeledCancel :: Generic LabeledCancel _
derive instance eqLabeledCancel :: Eq LabeledCancel
derive instance ordLabeledCancel :: Ord LabeledCancel
instance showLabeledCancel :: Show LabeledCancel where
  show = genericShow
instance boundedLabeledCancel :: Bounded LabeledCancel where
  top = genericTop
  bottom = genericBottom
instance boundedEnumLabeledCancel :: Enum LabeledCancel where
  succ = genericSucc
  pred = genericPred
