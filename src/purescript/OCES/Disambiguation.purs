module OCES.Disambiguation where

import Prelude

import Data.Bounded.Generic (genericTop, genericBottom)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data DisambiguationDirection
  = Top
  | Right
  | Bottom
  | Left

derive instance genericDisambiguationDirection :: Generic DisambiguationDirection _
derive instance eqDisambiguationDirection :: Eq DisambiguationDirection
derive instance ordDisambiguationDirection :: Ord DisambiguationDirection
instance showDisambiguationDirection :: Show DisambiguationDirection where
  show = genericShow
instance boundedDisambiguationDirection :: Bounded DisambiguationDirection where
  top = genericTop
  bottom = genericBottom
instance boundedEnumDisambiguationDirection :: Enum DisambiguationDirection where
  succ = genericSucc
  pred = genericPred


