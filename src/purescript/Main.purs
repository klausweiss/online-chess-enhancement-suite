module Main where

import Prelude

import Data.Bounded.Generic (genericTop, genericBottom)
import Data.Enum (class Enum, upFromIncluding)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromMaybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import Keyboard (Keycode, keycodeFor, shiftKey)
import Signal (Signal, constant, mergeMany, runSignal, (~>))
import Signal.DOM (keyPressed)
import Signal.Effect (foldEffect)


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

type State = Int

newtype Config = Config Keymap

movePiece :: Piece -> State -> Effect State
movePiece piece state = do
  log $ "moving piece: " <> show piece
  pure (state + 1)

movePieceSignal :: Keymap -> Piece -> Effect (Signal Piece)
movePieceSignal km p = do
  let key = km.pieceKey p
  log $ show key <> " <<- subscribing for this key"
  keyPressSignal <- keyPressed key
  pure (const p <$> keyPressSignal)

movePiecesSignal :: Keymap -> Effect (Signal Piece)
movePiecesSignal km = let
      allPieces = upFromIncluding bottom 
      allSignals :: Effect (Array (Signal Piece))
      allSignals = traverse (movePieceSignal km) allPieces
   in do
    mergedSignals <- mergeMany <$> allSignals
    pure $ fromMaybe (constant King) mergedSignals

main :: Effect Unit
main = do
  let keymap = defaultKeymap
  let initialState = 0
  moveSignal <- movePiecesSignal keymap
  todo <- foldEffect movePiece initialState moveSignal
  runSignal $ todo ~> (show >>> log)
  log "sup, done"

showB :: Boolean -> Effect Unit
showB = show >>> log
