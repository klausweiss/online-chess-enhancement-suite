module Main where

import Prelude

import Chess (Piece(..))
import Control.Monad.Maybe.Trans (runMaybeT)
import Data.Enum (upFromIncluding)
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Keyboard (Keycode, keycodeFor, shiftKey, altKey)
import Lichess as Lichess
import Signal (Signal, constant, filter, mergeMany)
import Signal.DOM (mousePos, CoordinatePair)
import Signal.DOM.Prevented (keyPressed)
import Signal.Effect (foldEffect)


type Keymap =
  { pieceKey :: Piece -> Keycode
  , topPiece :: Keycode
  , bottomPiece :: Keycode
  , leftPiece :: Keycode
  , rightPiece :: Keycode
  }


defaultKeymap :: Keymap
defaultKeymap =
  let pieceKey Pawn = keycodeFor 'w' 
      pieceKey Rook = keycodeFor 'a' 
      pieceKey Knight = keycodeFor 's' 
      pieceKey Bishop = keycodeFor 'd' 
      pieceKey King = shiftKey
      pieceKey Queen = altKey
   in
  { pieceKey: pieceKey
  -- TODO: a data type for the below
  , topPiece: keycodeFor 'w'
  , bottomPiece: keycodeFor 's'
  , leftPiece: keycodeFor 'a'
  , rightPiece: keycodeFor 'd'
  }

type State = 
  { pointerPosition :: CoordinatePair
  }

newtype Config = Config Keymap

movePieceSignal :: Keymap -> Piece -> Effect (Signal Piece)
movePieceSignal km p = do
  let key = km.pieceKey p
  keyPressSignal <- keyPressed true key
  let isKeyDown = eq true
  -- TODO: keyUp for keys which support it
  pure (const p <$> filter isKeyDown true keyPressSignal) -- only keydowns, true is essential for SHIFT to work

movePiecesSignal :: Keymap -> Effect (Signal Piece)
movePiecesSignal km = let
      allPieces = upFromIncluding bottom 
      allSignals :: Effect (Array (Signal Piece))
      allSignals = traverse (movePieceSignal km) allPieces
   in do
    mergedSignals <- mergeMany <$> allSignals
    pure $ fromMaybe (constant King) mergedSignals


data AppSignal 
  = MovePiece Piece
  | MovePointer CoordinatePair

processSignal :: AppSignal -> State -> Effect State
processSignal (MovePiece p) s = movePiece p s
processSignal (MovePointer coords) s = do
  pure $ s { pointerPosition = coords }

movePiece :: Piece -> State -> Effect State
movePiece piece state = do
  _ <- runMaybeT $ do
     square <- Lichess.coordsToSquare state.pointerPosition
     Lichess.moveRandomPieceToSquare piece square
  pure state

main :: Effect Unit
main = do
  let keymap = defaultKeymap
  let initialState = { pointerPosition: {x: 0, y: 0} }
  movePiecesSignal' <- movePiecesSignal keymap
  movePointerSignal' <- mousePos
  let sig = (MovePointer <$> movePointerSignal')
         <> (MovePiece <$> movePiecesSignal')
  _ <- foldEffect processSignal initialState sig :: Effect (Signal State)
  Lichess.enablePlugin

