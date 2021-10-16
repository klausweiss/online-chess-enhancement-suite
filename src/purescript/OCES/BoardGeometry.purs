module OCES.BoardGeometry where

import Prelude

import Data.Maybe (Maybe(..))
import OCES.Chess (File, Rank, Square(..), fileFromIndex, fileToIndex, rankFromIndex, oppositeSquare)


type Size2d = {width :: Int, height :: Int}
type CoordinatePair = {x :: Int, y :: Int}

yToIndex :: Int -> Int -> Maybe Int
yToIndex height y = 
   let 
       squareSize = height / 8 
       index = (y / squareSize) 
    in if index >= 0 && index <= 7 then Just index else Nothing

yToRank :: Int -> Int -> Maybe Rank
yToRank height y = yToIndex height y <#> rankFromIndex

xToFile :: Int -> Int -> Maybe File
xToFile width x = yToIndex width x >>= fileFromIndex

data Orientation = WhiteDown | BlackDown

derive instance eqOrientation :: Eq Orientation


getSquareCenterCoords :: Size2d -> Orientation -> Square -> CoordinatePair
getSquareCenterCoords boardSize BlackDown s = getSquareCenterCoords boardSize WhiteDown (oppositeSquare s)
getSquareCenterCoords boardSize WhiteDown (Square file rank) = 
   let 
       squareWidth = boardSize.width / 8
       halfSquareWidth = squareWidth / 2
    in { x: (fileToIndex file) * squareWidth + halfSquareWidth
       , y: (8 - rank) * squareWidth + halfSquareWidth
       } 
