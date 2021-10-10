module OCES.Lichess where

import Prelude

import OCES.BoardGeometry (Orientation(..), Size2d, getSquareCenterCoords, xToFile, yToRank, oppositeSquare)
import OCES.Chess (Color(..), Piece(..), PieceOnBoard(..), PlayerPiece(..), SimplePosition, Square(..), findPossibleMoveTargets, makeSimplePosition)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, find, head)
import Data.Array.NonEmpty ((!!))
import Data.Int (fromString, round)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex, match)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import OCES.Lichess.Plugin as Plugin
import Signal.DOM (CoordinatePair)
import Web.DOM.DOMTokenList (contains, DOMTokenList)
import Web.DOM.Element (Element, classList, clientHeight, clientWidth, fromNode, getAttribute, toEventTarget)
import Web.DOM.Element as Element
import Web.DOM.Node (Node)
import Web.DOM.NodeList (toArray)
import Web.DOM.ParentNode (ParentNode, QuerySelector(..), querySelector, querySelectorAll)
import Web.Event.EventTarget (dispatchEvent)
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (fromElement, getBoundingClientRect)
import Web.HTML.Window (document)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.MouseEvent.Constructors (makeMouseEvent)
import Web.UIEvent.MouseEvent.EventTypes (mousedown, mouseup)

coordsToSquare :: CoordinatePair -> MaybeT Effect Square
coordsToSquare coords = do
   doc <- lift $ window >>= document <#> HTMLDocument.toParentNode
   board <- getBoardElement doc
   orientation <- getOrientation doc
   htmlBoard <- MaybeT <<< pure $ fromElement board
   bCoords <- lift $ boardCoords htmlBoard
   let relativeCoords = coords - bCoords
   boardSize <- lift $ getBoardSize board
   boardCoordsToSquare boardSize orientation relativeCoords

boardCoordsToSquare :: Size2d -> Orientation -> CoordinatePair -> MaybeT Effect Square
boardCoordsToSquare boardSize orientation {x: relativeX, y: relativeY} = do
   squareWhiteDown <- MaybeT $ pure do
     file <- xToFile boardSize.width relativeX
     rank <- yToRank boardSize.height (boardSize.height - relativeY)
     pure $ Square file rank
   pure $ if orientation == WhiteDown then squareWhiteDown else oppositeSquare squareWhiteDown

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
   board <- MaybeT $ querySelector (QuerySelector ".cg-wrap") doc
   classes <- lift $ classList board
   isBlackDown <- lift $ contains classes "orientation-black"
   pure $ if isBlackDown then BlackDown else WhiteDown


getCurrentPosition :: MaybeT Effect SimplePosition
getCurrentPosition = do
   doc <- lift $ window >>= document <#> HTMLDocument.toParentNode
   board <- getBoardElement doc
   orient <- getOrientation doc
   bSize <- lift <<< getBoardSize $ board
   piecesNodes <- lift $ querySelectorAll (QuerySelector "piece") (Element.toParentNode board) >>= toArray :: MaybeT Effect (Array Node)
   piecesElements <- traverse (MaybeT <<< pure <<< fromNode) piecesNodes :: MaybeT Effect (Array Element)
   maybePieces <- lift <<< sequence $ (runMaybeT <<< pieceFromElement bSize orient) <$> piecesElements 
   let pieces = catMaybes maybePieces
   pure $ makeSimplePosition pieces

pieceFromElement :: Size2d -> Orientation -> Element -> MaybeT Effect PieceOnBoard
pieceFromElement boardSize orient el = do
   color <- colorFromElement el
   piece <- pieceTypeFromElement el
   square <- getSquareFromElement boardSize orient el
   let playerPiece = PlayerPiece color piece
   MaybeT $ pure $ Just $ PieceOnBoard playerPiece square

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

getSquareFromElement :: Size2d -> Orientation -> Element -> MaybeT Effect Square
getSquareFromElement boardSize orient el = do
   let halfSquare = {x: boardSize.width / 16, y: boardSize.height / 16}
   style <- MaybeT $ getAttribute "style" el
   coords <- MaybeT <<< pure $ getCoordsFromStyleAttr style
   boardCoordsToSquare boardSize orient (coords + halfSquare)

coordsRegex :: Regex
coordsRegex = unsafeRegex "translate\\(\\s*(\\d+)(?:\\.\\d+)?px,\\s*(\\d+)(?:\\.\\d+)?px\\)" mempty

getCoordsFromStyleAttr :: String -> Maybe CoordinatePair
getCoordsFromStyleAttr style = do
   -- example style: "transform: translate(0.5px, 180.433px);"
   -- converted to                         0    , 180
   matches <- match coordsRegex style
   x <- (join $ matches !! 1) >>= fromString
   y <- (join $ matches !! 2) >>= fromString
   Just {x: x, y: y}


makeAMove :: Square -> Square -> MaybeT Effect Unit
makeAMove from to = do
   doc <- lift $ window >>= document <#> HTMLDocument.toParentNode
   board <- getBoardElement doc
   orientation <- getOrientation doc

   clickSquare orientation board from
   clickSquare orientation board to

clickSquare :: Orientation -> Element -> Square -> MaybeT Effect Unit
clickSquare orient board square = do
   boardSize <- lift $ getBoardSize board
   htmlBoard <- MaybeT <<< pure $ fromElement board
   bCoords <- lift $ boardCoords htmlBoard
   let squareCoords = getSquareCenterCoords boardSize orient square
   let absoluteCoords = squareCoords + bCoords
   let mouseEvent evtype = MouseEvent.toEvent $ makeMouseEvent evtype absoluteCoords.x absoluteCoords.y
   let boardTarget = toEventTarget board
   let mouseEventAtPoint evtype = lift $ dispatchEvent (mouseEvent evtype) boardTarget
   _triggeredDown <- mouseEventAtPoint mousedown
   _triggeredUp <- mouseEventAtPoint mouseup
   lift $ log $ "clicking " <> show square
   pure unit


moveRandomPieceToSquare :: Piece -> Square -> MaybeT Effect Unit
moveRandomPieceToSquare p dest = do
   doc <- lift $ window >>= document <#> HTMLDocument.toParentNode
   orientation <- getOrientation doc
   position <- getCurrentPosition
   let color = if orientation == BlackDown then Black else White
   let possibleTargets = findPossibleMoveTargets color p dest position
   lift $ log $ show possibleTargets
   (PieceOnBoard _ source) <- MaybeT $ pure $ head possibleTargets
   makeAMove source dest


enablePlugin :: Effect Unit
enablePlugin = Plugin.enablePlugin

