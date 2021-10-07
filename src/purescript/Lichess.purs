module Lichess where

import Prelude

import BoardGeometry (Orientation(..), Size2d, xToFile, yToRank)
import Chess (Color(..), Piece(..), PieceOnBoard(..), PlayerPiece(..), SimplePosition, Square(..), makeSimplePosition, oppositeSquare)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, find)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Signal.DOM (CoordinatePair)
import Web.DOM.DOMTokenList (contains, DOMTokenList)
import Web.DOM.Element (Element, classList, clientHeight, clientWidth, fromNode)
import Web.DOM.Node (Node)
import Web.DOM.NodeList (toArray)
import Web.DOM.ParentNode (ParentNode, QuerySelector(..), querySelector, querySelectorAll)
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.HTMLElement (fromElement, getBoundingClientRect)
import Web.HTML.Window (document)

coordsToSquare :: CoordinatePair -> MaybeT Effect Square
coordsToSquare coords = do
   doc <- lift $ window >>= document <#> toParentNode
   board <- getBoardElement doc
   htmlBoard <- MaybeT <<< pure $ fromElement board
   bCoords <- lift $ boardCoords htmlBoard
   size <- lift $ getBoardSize board
   let {x: relativeX, y: relativeY} = coords - bCoords
   squareWhiteDown <- MaybeT $ pure do
     file <- xToFile size.width relativeX
     rank <- yToRank size.height (size.height - relativeY)
     pure $ Square file rank
   o <- getOrientation doc
   pure $ if o == WhiteDown then squareWhiteDown else oppositeSquare squareWhiteDown

sizeToCoords :: forall r. { height :: Number , width :: Number | r } -> { x :: Int , y :: Int }
sizeToCoords s = {x: round s.width, y: round s.height}

getBoardElement :: ParentNode -> MaybeT Effect Element
getBoardElement doc = MaybeT $ querySelector (QuerySelector "cg-board") doc

getBoardSize :: Element -> Effect Size2d
getBoardSize b = {width : _, height : _} <$> (clientWidth b <#> round) <*> (clientHeight b <#> round)

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


getCurrentPosition :: MaybeT Effect SimplePosition
getCurrentPosition = do
   doc <- lift $ window >>= document <#> toParentNode
   bSize <- getBoardElement doc >>= (lift <<< getBoardSize)
   piecesNodes <- lift $ querySelectorAll (QuerySelector "piece") doc >>= toArray :: MaybeT Effect (Array Node)
   piecesElements <- traverse (MaybeT <<< pure <<< fromNode) piecesNodes :: MaybeT Effect (Array Element)
   maybePieces <- lift <<< sequence $ (runMaybeT <<< pieceFromElement bSize) <$> piecesElements 
   let pieces = catMaybes maybePieces
   pure $ makeSimplePosition pieces

pieceFromElement :: Size2d -> Element -> MaybeT Effect PieceOnBoard
pieceFromElement boardSize el = do
   color <- colorFromElement el
   piece <- pieceTypeFromElement el
   square <- getSquareFromElement boardSize el
   let playerPiece = PlayerPiece color piece
   x <- MaybeT $ pure $ Just $ PieceOnBoard playerPiece square
   lift <<< log $ show x
   pure x

colorFromElement :: Element -> MaybeT Effect Color
colorFromElement el = do
   classes <- lift $ classList el
   isBlack <- lift $ contains classes "black"
   let color = if isBlack then Black else White
   MaybeT $ pure $ Just color

pieceTypeFromElement :: Element -> MaybeT Effect Piece
pieceTypeFromElement el = let 
   firstMatch :: Array (Tuple String Piece) -> DOMTokenList -> MaybeT Effect Piece
   firstMatch pairs classes = 
      let 
         matchPair (Tuple string piece) = do
            isOk <- contains classes string
            pure $ Tuple isOk piece
       in do
          matches <- lift $ traverse matchPair pairs
          MaybeT <<< pure <<< map snd $ find fst matches 
   in do
   classes <- lift $ classList el
   pieceType <- firstMatch 
      [ Tuple "pawn" Pawn
      , Tuple "rook" Rook
      , Tuple "bishop" Bishop
      , Tuple "knight" Knight
      , Tuple "queen" Queen
      , Tuple "king" King
      ] classes
   MaybeT <<< pure <<< Just $ pieceType

getSquareFromElement :: Size2d -> Element -> MaybeT Effect Square
getSquareFromElement boardSize el = do
   MaybeT $ pure $ Just (Square 'a' 4) -- TODO

