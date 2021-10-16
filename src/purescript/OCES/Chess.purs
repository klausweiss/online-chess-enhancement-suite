module OCES.Chess where

import Prelude

import Data.Array (filter, partition)
import Data.Bounded.Generic (genericTop, genericBottom)
import Data.Char (fromCharCode, toCharCode)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Ord (abs)
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

rankToIndex :: Rank -> Int
rankToIndex r = r - 1

fileToIndex :: File -> Int
fileToIndex f = toCharCode f - 97

rankFromIndex :: Int -> Rank
rankFromIndex n = n + 1

fileFromIndex :: Int -> Maybe File
fileFromIndex n = fromCharCode (n + 97)

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


findPossibleMoveTargets :: Color -> Piece -> Square -> SimplePosition -> Array PieceOnBoard
findPossibleMoveTargets c p dest pos = 
  let
      possiblePieces = filter (canMoveToSquare dest pos)
        $ filter (\(PieceOnBoard (PlayerPiece _ piece) _) -> piece == p) 
        $ if c == White then pos.white else pos.black
   in
    possiblePieces

canMoveToSquare :: Square -> SimplePosition -> PieceOnBoard -> Boolean
canMoveToSquare toSquare pos (PieceOnBoard (PlayerPiece White piece) fromSquare) = canPieceMoveToSquare (indexSquare $ fromSquare) (indexSquare $ toSquare) piece pos
canMoveToSquare toSquare pos (PieceOnBoard (PlayerPiece Black piece) fromSquare) = canPieceMoveToSquare (indexSquare $ oppositeSquare fromSquare) (indexSquare $ oppositeSquare toSquare) piece pos

type FileIndex = Int
type RankIndex = Int
data IndexSquare = IndexSquare FileIndex RankIndex

indexSquare :: Square -> IndexSquare
indexSquare (Square f r) = IndexSquare (fileToIndex f) (rankToIndex r)

-- TODO
-- assumes white color
canPieceMoveToSquare :: IndexSquare -> IndexSquare -> Piece -> SimplePosition -> Boolean
canPieceMoveToSquare (IndexSquare fromFile fromRank) (IndexSquare toFile toRank) Pawn pos = 
  case fromFile, toFile of 
       f, t | f == t -> (fromRank == 1 && toRank == 3) || (toRank - fromRank) == 1
       f, t | abs (f - t) == 1 -> (toRank - fromRank) == 1
       _, _ -> false
canPieceMoveToSquare (IndexSquare fromFile fromRank) (IndexSquare toFile toRank) Rook pos = true
canPieceMoveToSquare (IndexSquare fromFile fromRank) (IndexSquare toFile toRank) Knight pos = true
canPieceMoveToSquare (IndexSquare fromFile fromRank) (IndexSquare toFile toRank) Bishop pos = true
canPieceMoveToSquare (IndexSquare fromFile fromRank) (IndexSquare toFile toRank) King pos = true
canPieceMoveToSquare (IndexSquare fromFile fromRank) (IndexSquare toFile toRank) Queen pos = true
