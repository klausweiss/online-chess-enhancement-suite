module Chess where

import Prelude

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


type Rank = Int
type File = Char
data Square = Square File Rank

derive instance genericSquare :: Generic Square _
derive instance eqSquare :: Eq Square
derive instance ordSquare :: Ord Square

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

