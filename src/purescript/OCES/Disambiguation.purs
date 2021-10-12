module OCES.Disambiguation where

import Prelude

import Data.Array (length, sortWith, take, takeEnd, reverse, drop, dropEnd, last, takeWhile)
import Data.Bounded.Generic (genericTop, genericBottom)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Int (ceil, toNumber)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import OCES.BoardGeometry (fileToRank, Orientation(..))
import OCES.Chess (PieceOnBoard(..), Square(..))

data DisambiguationDirection
  = Top
  | Right
  | Bottom
  | Left

derive instance genericDisambiguationDirection :: Generic DisambiguationDirection _
derive instance eqDisambiguationDirection :: Eq DisambiguationDirection
derive instance ordDisambiguationDirection :: Ord DisambiguationDirection
instance showDisambiguationDirection :: Show DisambiguationDirection where
  show = genericShow
instance boundedDisambiguationDirection :: Bounded DisambiguationDirection where
  top = genericTop
  bottom = genericBottom
instance boundedEnumDisambiguationDirection :: Enum DisambiguationDirection where
  succ = genericSucc
  pred = genericPred


horizontally (PieceOnBoard _ (Square file _rank)) = fileToRank file
vertically (PieceOnBoard _ (Square _file rank)) = rank

direction Top = vertically 
direction Right = horizontally 
direction Bottom = vertically 
direction Left = horizontally 

type ArrayPartition = 
  { take :: forall a. Int -> Array a -> Array a 
  , drop :: forall a. Int -> Array a -> Array a 
  }

takePart Top = { take: takeEnd, drop: dropEnd }
takePart Right = { take: takeEnd, drop: dropEnd }
takePart Bottom = { take: take, drop: drop }
takePart Left = { take: take, drop: drop }

-- TODO: don't take the exact half, but also include the pieces with metric equal to the middle one
filterByDirection :: Orientation -> DisambiguationDirection -> Array PieceOnBoard -> Array PieceOnBoard
filterByDirection orient d pieces = 
  let piecesCnt = length pieces
      halfPiecesCntUp = ceil $ (toNumber piecesCnt) / 2.0
      sortedPieces = sortWith dir pieces
      piecesInOrder = if orient == WhiteDown then sortedPieces else reverse sortedPieces
      arrayPartition = takePart d
      dir = direction d
      lowerPart = arrayPartition.take halfPiecesCntUp piecesInOrder
      upperPart = arrayPartition.drop halfPiecesCntUp piecesInOrder
      piecesEqualToLast = takeMatchingElements (last lowerPart) dir upperPart
   in lowerPart <> piecesEqualToLast

takeMatchingElements :: forall a b. Eq b => Maybe a -> (a -> b) -> Array a -> Array a
takeMatchingElements Nothing _ _ = []
takeMatchingElements (Just x) f ys = 
  let 
      fx = f x 
   in takeWhile (\y -> f y == fx) ys
