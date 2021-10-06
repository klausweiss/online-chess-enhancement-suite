module Lichess where

import Prelude

import Chess (ChessboardColumn, ChessboardRow, Square(..))
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Signal.DOM (CoordinatePair)
import Web.DOM (Element)
import Web.DOM.Element (clientHeight, clientWidth)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.HTMLElement (fromElement, getBoundingClientRect)
import Web.HTML.Window (document)

coordsToSquare :: CoordinatePair -> Effect (Maybe Square)
coordsToSquare coords = do
  doc <- window >>= document <#> toParentNode
  maybeSquare <- runMaybeT $ do
     board <- MaybeT $ querySelector (QuerySelector "cg-board") doc :: MaybeT Effect Element
     htmlBoard <- MaybeT <<< pure $ fromElement board
     bCoords <- lift $ boardCoords htmlBoard
     size <- lift $ boardSize board
     let {x: relativeX, y: relativeY} = coords - bCoords
     MaybeT $ pure do
        col <- xToColumn size.width relativeX
        row <- yToRow size.height (size.height - relativeY)
        pure $ Square col row
  pure maybeSquare

type Size2d = {width :: Int, height :: Int}

sizeToCoords :: forall r. { height :: Number , width :: Number | r } -> { x :: Int , y :: Int }
sizeToCoords s = {x: round s.width, y: round s.height}

boardSize :: Element -> Effect Size2d
boardSize b = {width : _, height : _} <$> (clientWidth b <#> round) <*> (clientHeight b <#> round)

boardCoords :: HTMLElement -> Effect CoordinatePair
boardCoords b = do
  rect <- getBoundingClientRect b
  pure { x : round rect.left, y : round rect.top}

yToRow :: Int -> Int -> Maybe ChessboardRow
yToRow height y = let 
   squareSize = height / 8
   row = (y / squareSize) + 1
  in if row > 0 && row < 9 then Just row else Nothing

xToColumn :: Int -> Int -> Maybe ChessboardColumn
xToColumn width y = yToRow width y >>= rowToChar

rowToChar :: Int -> Maybe Char
rowToChar 1 = Just 'a'
rowToChar 2 = Just 'b'
rowToChar 3 = Just 'c'
rowToChar 4 = Just 'd'
rowToChar 5 = Just 'e'
rowToChar 6 = Just 'f'
rowToChar 7 = Just 'g'
rowToChar 8 = Just 'h'
rowToChar _ = Nothing


