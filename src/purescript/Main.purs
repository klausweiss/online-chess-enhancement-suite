module Main where

import Prelude

import Lichess as Lichess
import Chess (Piece(..), Square)
import Data.Enum (upFromIncluding)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import Keyboard (Keycode, keycodeFor, shiftKey)
import Signal (Signal, constant, filter, mergeMany, (~>), unwrap)
import Signal.DOM (keyPressed, mousePos, CoordinatePair)
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
      pieceKey Queen = keycodeFor ' ' 
   in
  { pieceKey: pieceKey
  -- TODO: a data type for the below
  , topPiece: keycodeFor 'w'
  , bottomPiece: keycodeFor 's'
  , leftPiece: keycodeFor 'a'
  , rightPiece: keycodeFor 'd'
  }

type State = 
  { initializedMovePiece :: Boolean -- foldEffect yields a Signal once when called... 
  , squareUnderPointer :: Maybe Square
  }

newtype Config = Config Keymap

movePieceSignal :: Keymap -> Piece -> Effect (Signal Piece)
movePieceSignal km p = do
  let key = km.pieceKey p
  log $ "subscribing " <> show p <> " for key " <> show key
  keyPressSignal <- keyPressed key
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

pointOnSquareSignal :: (CoordinatePair -> Effect (Maybe Square)) -> Effect (Signal (Maybe Square))
pointOnSquareSignal coordsToSquare = do
  mouseSignal <- mousePos
  unwrap (mouseSignal ~> coordsToSquare)


data AppSignal 
  = MovePiece Piece
  | PointOnSquare (Maybe Square)

processSignal :: AppSignal -> State -> Effect State
processSignal (MovePiece p) s = movePiece p s
processSignal (PointOnSquare ms) s = do
  pure s

movePiece :: Piece -> State -> Effect State
movePiece _ state@{ initializedMovePiece: false } = pure $ state {initializedMovePiece = true}
movePiece piece state = do
  log $ "moving piece: " <> show piece <> ", state: " <> show state
  pure state

main :: Effect Unit
main = do
  let keymap = defaultKeymap
  let initialState = {initializedMovePiece: false, squareUnderPointer: Nothing}
  movePiecesSignal' <- movePiecesSignal keymap
  pointOnSquareSignal' <- pointOnSquareSignal Lichess.coordsToSquare
  let sig = (MovePiece <$> movePiecesSignal')
         <> (PointOnSquare <$> pointOnSquareSignal')
  log "about to fold"
  _ <- foldEffect processSignal initialState sig :: Effect (Signal State)
  log "folded"


-- notes:
-- - clicking on a piece in lichess:
--   - click on the board element, by triggering a click MouseEvent, bubble: true
--   - get the board element from document.elementFromPoint
--   - 
