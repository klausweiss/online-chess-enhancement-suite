module Chess where

import Prelude

import Data.Array (filter, partition)
import Data.Bounded.Generic (genericTop, genericBottom)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)


data Piece 
  = Pawn
  | Rook
  | Knight
  | Bishop
  | King
  | Queen

derive instance genericPiece :: Generic Piece _
derive instance eqPiece :: Eq Piece
derive instance ordPiece :: Ord Piece
instance showPiece :: Show Piece where
  show = genericShow
instance boundedPiece :: Bounded Piece where
  top = genericTop
  bottom = genericBottom
instance boundedEnumPiece :: Enum Piece where
  succ = genericSucc
  pred = genericPred


data Color
  = Black
  | White

derive instance genericColor :: Generic Color _
derive instance eqColor :: Eq Color
derive instance ordColor :: Ord Color
instance showColor :: Show Color where
  show = genericShow
instance boundedColor :: Bounded Color where
  top = genericTop
  bottom = genericBottom
instance boundedEnumColor :: Enum Color where
  succ = genericSucc
  pred = genericPred


data PlayerPiece = PlayerPiece Color Piece

derive instance genericPlayerPiece :: Generic PlayerPiece _
derive instance eqPlayerPiece :: Eq PlayerPiece
instance showPlayerPiece :: Show PlayerPiece where
  show = genericShow


type Rank = Int
type File = Char
data Square = Square File Rank

derive instance genericSquare :: Generic Square _
derive instance eqSquare :: Eq Square
instance showSquare :: Show Square where
  show = genericShow

oppositeSquare :: Square -> Square
oppositeSquare (Square f r) = Square (oppositeFile f) (9 - r)

oppositeFile :: File -> File
oppositeFile 'a' = 'h'
oppositeFile 'b' = 'g'
oppositeFile 'c' = 'f'
oppositeFile 'd' = 'e'
oppositeFile 'e' = 'd'
oppositeFile 'f' = 'c'
oppositeFile 'g' = 'b'
oppositeFile 'h' = 'a'
oppositeFile _ = 'x'


data PieceOnBoard = PieceOnBoard PlayerPiece Square

derive instance genericPieceOnBoard :: Generic PieceOnBoard _
derive instance eqPieceOnBoard :: Eq PieceOnBoard
instance showPieceOnBoard :: Show PieceOnBoard where
  show = genericShow

-- no castling rights nor en passant information
type SimplePosition =
  { black :: Array PieceOnBoard
  , white :: Array PieceOnBoard
  }

makeSimplePosition :: Array PieceOnBoard -> SimplePosition
makeSimplePosition pieces = let
    {yes: w, no: b} = partition (\(PieceOnBoard (PlayerPiece color _) _) -> color == White) pieces
  in { black: b, white: w }

