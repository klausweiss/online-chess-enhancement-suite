module OCES.Preferences.Components.EnumSlider where

import Prelude

import Control.Monad.State (get, put)
import Data.Array (elemIndex, length, (!!))
import Data.Int (toNumber, fromString)
import Data.List (toUnfoldable)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


type LabeledValue v =
  { value :: v
  , long :: String
  }

type Input v =
  { possibleValues :: NonEmptyList.NonEmptyList (LabeledValue v)
  , defaultValue :: Maybe v
  , label :: String
  }

type State v =
  { possibleValues :: Array (LabeledValue v)
  , selectedValue :: v
  , label :: String
  }

data Query v a
  = GetSelectedValue (v -> a)

data Action v
  = SetNewValue (Maybe v)


enumSlider :: forall v output m. Eq v => H.Component (Query v) (Input v) output m
enumSlider = 
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
    }


initialState :: forall v. Input v -> State v
initialState state =
  { possibleValues: toUnfoldable <<< NonEmptyList.toList $ state.possibleValues
  , selectedValue: 
      fromMaybe 
        (NonEmptyList.head state.possibleValues # (_.value)) 
        state.defaultValue
  , label: state.label
  }

render :: forall m v. Eq v => State v -> H.ComponentHTML (Action v) () m
render state =
  let 
      gridClass = ClassName "grid"
      rowClass = ClassName "row"
      columnClass = ClassName "column"

      parseValue :: String -> Maybe v
      parseValue s = (_.value) <$> (fromString s <#> (_-1) >>= (state.possibleValues !! _))

      selectedIndex = fromMaybe 0 $ elemIndex state.selectedValue ((_.value) <$> state.possibleValues)
      selectedItem = state.possibleValues !! selectedIndex
      input = HH.input
        [ HP.type_ HP.InputRange
        , HP.min $ toNumber 1
        , HP.max $ toNumber (length state.possibleValues)
        , HP.value <<< show <<< toNumber $ selectedIndex + 1
        , HP.class_ columnClass
        , HE.onValueInput (parseValue >>> SetNewValue)
        ]
      label = HH.label 
        [ HP.classes $ [ columnClass, ClassName "label" ] ]
        [ HH.text $ state.label ]
      valueLabel = HH.span 
        [ HP.classes $ [ columnClass, ClassName "valueLabel" ] ]
        [ HH.text $ fromMaybe "invalid value" ((_.long) <$> selectedItem) ]
   in HH.div 
    [ HP.classes $ [ gridClass, ClassName "enumSlider" ] ]
    [ HH.div
      [ HP.class_ rowClass ]
      [ input, label ]
    , HH.div
      [ HP.class_ rowClass ]
      [ valueLabel ]
    ]

handleAction :: forall v m output. Action v -> H.HalogenM (State v) (Action v) () output m Unit
handleAction = 
  case _ of
  SetNewValue Nothing -> do
    pure unit
  SetNewValue (Just value) -> do
    state <- get
    put $ state { selectedValue = value } 

handleQuery :: forall v output a m. Query v a -> H.HalogenM (State v) (Action v) () output m (Maybe a)
handleQuery = 
  case _ of
  GetSelectedValue reply -> do
    state <- get
    pure <<< Just <<< reply $ state.selectedValue
