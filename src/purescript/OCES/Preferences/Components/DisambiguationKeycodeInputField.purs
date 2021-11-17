module OCES.Preferences.Components.DisambiguationKeycodeInputField where

import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Halogen (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import OCES.Disambiguation (DisambiguationDirection)
import OCES.Preferences.Components.KeycodeInputField as KeycodeInputField


newtype LabeledDisambiguationDirection = LabeledDisambiguationDirection DisambiguationDirection
instance KeycodeInputField.HtmlLabel LabeledDisambiguationDirection where
  htmlLabel (LabeledDisambiguationDirection d) = 
    HH.div 
      [ HP.classes [ ClassName "icon", ClassName "direction", ClassName (show d) ]
      , HP.title $ show d
      ] []

derive instance genericLabeledDisambiguationDirection :: Generic LabeledDisambiguationDirection _
derive instance eqLabeledDisambiguationDirection :: Eq LabeledDisambiguationDirection
derive instance ordLabeledDisambiguationDirection :: Ord LabeledDisambiguationDirection
instance showLabeledDisambiguationDirection :: Show LabeledDisambiguationDirection where
  show = genericShow
instance boundedLabeledDisambiguationDirection :: Bounded LabeledDisambiguationDirection where
  top = genericTop
  bottom = genericBottom
instance boundedEnumLabeledDisambiguationDirection :: Enum LabeledDisambiguationDirection where
  succ = genericSucc
  pred = genericPred
