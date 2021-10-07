module BoardGeometry where

import Prelude

import Chess (File, Rank)
import Data.Maybe (Maybe(..))


type Size2d = {width :: Int, height :: Int}

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


