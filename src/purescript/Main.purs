module Main where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Array (nub)
import Data.Enum (class Enum, upFromIncluding)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import OCES.Chess (Piece(..))
import OCES.Disambiguation (DisambiguationDirection(..))
import OCES.Keyboard (Keycode, keycodeFor, shiftKey, altKey)
import OCES.Lichess as Lichess
import Signal (Signal, constant, filter, mergeMany)
import Signal.DOM (mousePos, CoordinatePair)
import Signal.DOM.Prevented (keyPressed)
import Signal.Effect (foldEffect)


type Keymap =
  { pieceKey :: Piece -> Keycode
  , disambiguationKey :: DisambiguationDirection -> Keycode
  }


defaultKeymap :: Keymap
defaultKeymap =
  let pieceKey Pawn = keycodeFor 'w' 
      pieceKey Rook = keycodeFor 'a' 
      pieceKey Knight = keycodeFor 's' 
      pieceKey Bishop = keycodeFor 'd' 
      pieceKey King = shiftKey
      pieceKey Queen = altKey
      disambiguationKey Top = keycodeFor 'w'
      disambiguationKey Bottom = keycodeFor 's'
      disambiguationKey Left = keycodeFor 'a'
      disambiguationKey Right = keycodeFor 'd'
   in
  { pieceKey: pieceKey
  , disambiguationKey : disambiguationKey
  }

reverseMap :: forall e k. Enum e => Bounded e => Ord k => (e -> k) -> k -> Maybe e
reverseMap enumToKey = 
  let
      allEnums = upFromIncluding bottom :: Array e
      keysMap = fromFoldable $ (\e -> Tuple (enumToKey e) e) <$> allEnums :: Map k e
   in
    (flip lookup) keysMap

data ExpectedInput
  = PieceKey
  | DisambiguationKey

derive instance eqExpectedInput :: Eq ExpectedInput


type State = 
  { pointerPosition :: CoordinatePair
  , expectedInput :: ExpectedInput
  }

newtype Config = Config Keymap

keySignal :: Keycode -> Effect (Signal Keycode)
keySignal key = do
  signal <- keyPressed true key 
  -- TODO: keyUp for keys which support it
  let isKeyDown = eq true
  pure $ const key <$> filter isKeyDown true signal -- only keydowns, true is essential for SHIFT to work

keymapSignals :: Keymap -> Effect (Signal Keycode)
keymapSignals keymap = 
  let
      pieceKeys = keymap.pieceKey <$> upFromIncluding bottom
      disambiguationKeys = keymap.disambiguationKey <$> upFromIncluding bottom
      allKeys = nub $ pieceKeys <> disambiguationKeys
      allSignals = traverse keySignal allKeys
  in do
     mergedSignals <- mergeMany <$> allSignals
     pure $ fromMaybe (constant (-1)) mergedSignals


data AppSignal 
  = KeyPressSignal Keycode
  | MovePointer CoordinatePair

processSignal :: Keymap -> AppSignal -> State -> Effect State
processSignal keymap = 
  let 
    keyToPiece = reverseMap keymap.pieceKey
    keyToDisambiguation = reverseMap keymap.disambiguationKey

    processSignal' :: AppSignal -> State -> Effect State
    processSignal' (KeyPressSignal key) s@{ expectedInput: PieceKey } = do
      _ <- runMaybeT $ do
        piece <- MaybeT $ pure $ keyToPiece key
        lift $ movePiece piece s
      pure $ s
    processSignal' (KeyPressSignal key) s@{ expectedInput: DisambiguationKey } = do
      pure $ s
    processSignal' (MovePointer coords) s = do
      pure $ s { pointerPosition = coords }

   in processSignal'

movePiece :: Piece -> State -> Effect State
movePiece piece state = do
  _ <- runMaybeT $ do
     square <- Lichess.coordsToSquare state.pointerPosition
     Lichess.moveRandomPieceToSquare piece square
  pure state

main :: Effect Unit
main = do
  let keymap = defaultKeymap
  let initialState = { pointerPosition: {x: 0, y: 0}
                     , expectedInput: PieceKey
                     }
  keymapSignals' <- keymapSignals keymap
  movePointerSignal' <- mousePos
  let sig = (MovePointer <$> movePointerSignal')
         <> (KeyPressSignal <$> keymapSignals')
  _ <- foldEffect (processSignal keymap) initialState sig :: Effect (Signal State)
  Lichess.enablePlugin

