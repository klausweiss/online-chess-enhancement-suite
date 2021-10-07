module Lichess where

import Prelude

import BoardGeometry (Orientation(..), Size2d, xToFile, yToRank)
import Chess (Square(..), oppositeSquare)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Int (round)
import Data.Maybe (Maybe)
import Effect (Effect)
import Signal.DOM (CoordinatePair)
import Web.DOM (Element)
import Web.DOM.DOMTokenList (contains)
import Web.DOM.Element (classList, clientHeight, clientWidth)
import Web.DOM.ParentNode (ParentNode, QuerySelector(..), querySelector)
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.HTMLElement (fromElement, getBoundingClientRect)
import Web.HTML.Window (document)

coordsToSquare :: CoordinatePair -> MaybeT Effect Square
coordsToSquare coords = do
   doc <- lift $ window >>= document <#> toParentNode
   board <- MaybeT $ querySelector (QuerySelector "cg-board") doc :: MaybeT Effect Element
   htmlBoard <- MaybeT <<< pure $ fromElement board
   bCoords <- lift $ boardCoords htmlBoard
   size <- lift $ boardSize board
   let {x: relativeX, y: relativeY} = coords - bCoords
   squareWhiteDown <- MaybeT $ pure do
     file <- xToFile size.width relativeX
     rank <- yToRank size.height (size.height - relativeY)
     pure $ Square file rank
   o <- getOrientation doc
   pure $ if o == WhiteDown then squareWhiteDown else oppositeSquare squareWhiteDown

sizeToCoords :: forall r. { height :: Number , width :: Number | r } -> { x :: Int , y :: Int }
sizeToCoords s = {x: round s.width, y: round s.height}

boardSize :: Element -> Effect Size2d
boardSize b = {width : _, height : _} <$> (clientWidth b <#> round) <*> (clientHeight b <#> round)

boardCoords :: HTMLElement -> Effect CoordinatePair
boardCoords b = do
   rect <- getBoundingClientRect b
   pure { x : round rect.left, y : round rect.top}

getOrientation :: ParentNode -> MaybeT Effect Orientation
getOrientation doc = do
   filesElem <- MaybeT $ querySelector (QuerySelector "coords.files") doc
   classes <- lift $ classList filesElem
   isBlackDown <- lift $ contains classes "black"
   pure $ if isBlackDown then BlackDown else WhiteDown
