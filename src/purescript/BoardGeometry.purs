module BoardGeometry where

import Prelude

import Chess (ChessboardColumn, ChessboardRow)
import Data.Maybe (Maybe(..))


type Size2d = {width :: Int, height :: Int}

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


