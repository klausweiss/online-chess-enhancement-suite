module Lichess where

import Prelude

import Chess (Square)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Signal.DOM (CoordinatePair)

coordsToSquare :: CoordinatePair -> Effect (Maybe Square)
coordsToSquare {x, y} = do
  pure Nothing
