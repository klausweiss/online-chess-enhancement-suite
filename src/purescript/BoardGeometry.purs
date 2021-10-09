module BoardGeometry where

import Prelude

import Chess (File, Rank, Square(..))
import Data.Maybe (Maybe(..), fromMaybe)


type Size2d = {width :: Int, height :: Int}
type CoordinatePair = {x :: Int, y :: Int}

yToRank :: Int -> Int -> Maybe Rank
yToRank height y = let 
   squareSize = height / 8
   rank = (y / squareSize) + 1
  in if rank > 0 && rank < 9 then Just rank else Nothing

xToFile :: Int -> Int -> Maybe File
xToFile width y = yToRank width y >>= rankToChar

rankToChar :: Int -> Maybe Char
rankToChar 1 = Just 'a'
rankToChar 2 = Just 'b'
rankToChar 3 = Just 'c'
rankToChar 4 = Just 'd'
rankToChar 5 = Just 'e'
rankToChar 6 = Just 'f'
rankToChar 7 = Just 'g'
rankToChar 8 = Just 'h'
rankToChar _ = Nothing

charToRank :: Char -> Maybe Int
charToRank 'a' = Just 1
charToRank 'b' = Just 2
charToRank 'c' = Just 3
charToRank 'd' = Just 4
charToRank 'e' = Just 5
charToRank 'f' = Just 6
charToRank 'g' = Just 7
charToRank 'h' = Just 8
charToRank _ = Nothing

fileToRank :: Char -> Int
fileToRank = fromMaybe (-1) <<< charToRank

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

data Orientation = WhiteDown | BlackDown

derive instance eqOrientation :: Eq Orientation


getSquareCenterCoords :: Size2d -> Orientation -> Square -> CoordinatePair
getSquareCenterCoords boardSize BlackDown s = getSquareCenterCoords boardSize WhiteDown (oppositeSquare s)
getSquareCenterCoords boardSize WhiteDown (Square file rank) = 
   let 
       squareWidth = boardSize.width / 8
       halfSquareWidth = squareWidth / 2
    in { x: ((fileToRank file) - 1) * squareWidth + halfSquareWidth
       , y: (8 - rank) * squareWidth + halfSquareWidth
       } 
