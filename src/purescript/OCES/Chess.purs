module OCES.Chess where

import Prelude

import Data.Array (filter, partition)
import Data.Bounded.Generic (genericTop, genericBottom)
import Data.Char (fromCharCode, toCharCode)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Foldable (find)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Ord (abs)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))


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

allPieces :: SimplePosition -> Array PieceOnBoard
allPieces sp = sp.black <> sp.white

flipChessboard :: SimplePosition -> SimplePosition
flipChessboard pos = makeSimplePosition $ (\(PieceOnBoard pp sq) -> (PieceOnBoard pp (oppositeSquare sq))) <$> allPieces pos


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
canMoveToSquare toSquare pos (PieceOnBoard (PlayerPiece Black piece) fromSquare) = canPieceMoveToSquare (indexSquare $ oppositeSquare fromSquare) (indexSquare $ oppositeSquare toSquare) piece (flipChessboard pos)

type FileIndex = Int
type RankIndex = Int
data IndexSquare = IndexSquare FileIndex RankIndex

derive instance eqIndexSquare :: Eq IndexSquare
derive instance ordIndexSquare :: Ord IndexSquare

indexSquare :: Square -> IndexSquare
indexSquare (Square f r) = IndexSquare (fileToIndex f) (rankToIndex r)

-- assumes white color
canPieceMoveToSquare :: IndexSquare -> IndexSquare -> Piece -> SimplePosition -> Boolean
canPieceMoveToSquare (IndexSquare fromFile fromRank) to@(IndexSquare toFile toRank) Pawn pos = 
  let 
      occupied = isOccupied to pos 
   in case fromFile, toFile of 
       f, t | f == t -> (fromRank == 1 && toRank == 3) || (toRank - fromRank) == 1 && not occupied
       f, t | abs (f - t) == 1 -> (toRank - fromRank) == 1 && occupied
       _, _ -> false
canPieceMoveToSquare f@(IndexSquare fromFile fromRank) t@(IndexSquare toFile toRank) Rook pos 
  | (fromFile == toFile || fromRank == toRank) = nothingBetween f t pos
canPieceMoveToSquare (IndexSquare fromFile fromRank) (IndexSquare toFile toRank) Knight _
  | (abs (fromFile - toFile) == 2 && abs (fromRank - toRank) == 1) 
  || (abs (fromFile - toFile) == 1 && abs (fromRank - toRank) == 2) = true
canPieceMoveToSquare f@(IndexSquare fromFile fromRank) t@(IndexSquare toFile toRank) Bishop pos 
  | abs (fromFile - toFile) == abs(fromRank - toRank) = nothingBetween f t pos
canPieceMoveToSquare (IndexSquare fromFile fromRank) (IndexSquare toFile toRank) King _ 
  | abs (fromFile - toFile) <= 1 && abs (fromRank - toRank) <= 1 = true
canPieceMoveToSquare f t Queen pos = canPieceMoveToSquare f t Rook pos || canPieceMoveToSquare f t Bishop pos
canPieceMoveToSquare _ _ _ _ = false

nothingBetween :: IndexSquare -> IndexSquare -> SimplePosition -> Boolean
nothingBetween from to pos =
  let
      occupiedSquares = Map.fromFoldable $ allPieces pos <#> (\p@(PieceOnBoard _ s) -> Tuple (indexSquare s) p)
      start = if from < to then from else to
      end = if from > to then from else to
      go f t | f == t = true
      go f t = 
        let 
            maybeNs = nextSquare f t
            isNsEnd = fromMaybe false $ (\ns -> ns == t) <$> maybeNs
            isNsFree = fromMaybe false $ (\k -> not $ Map.member k occupiedSquares) <$> maybeNs
         in isNsEnd || (isNsFree && maybe false (\ns' -> go ns' t) maybeNs)
       in go start end

nextSquare :: IndexSquare -> IndexSquare -> Maybe IndexSquare
nextSquare (IndexSquare fromFile fromRank) (IndexSquare toFile toRank) | fromRank == toRank && fromFile < toFile = Just $ IndexSquare (fromFile + 1) fromRank
nextSquare (IndexSquare fromFile fromRank) (IndexSquare toFile toRank) | fromFile == toFile && fromRank < toRank = Just $ IndexSquare fromFile (fromRank + 1)
nextSquare (IndexSquare fromFile fromRank) (IndexSquare toFile toRank) | abs (toFile - fromFile) == abs (toRank - fromRank) = 
                                                                       let
                                                                           nextFile = (fromFile + (if toFile > fromFile then 1 else -1))
                                                                           nextRank = (fromRank + (if toRank > fromRank then 1 else -1))
                                                                        in Just $ IndexSquare nextFile nextRank
nextSquare _ _ = Nothing

isOccupied :: IndexSquare -> SimplePosition -> Boolean
isOccupied s pos = isJust $ find (\bs -> bs == s) $ allPieces pos <#> (\(PieceOnBoard _ bs) -> indexSquare bs)

