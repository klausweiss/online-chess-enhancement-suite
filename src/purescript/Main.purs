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
import Data.Unfoldable as Unfoldable
import Effect (Effect)
import OCES.Chess (PieceOnBoard(..), Piece(..), Square)
import OCES.Disambiguation (DisambiguationDirection(..), filterByDirection)
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

data InputState
  = NoInputYet
  | DisambiguationNeeded (Array PieceOnBoard)

derive instance eqInputState :: Eq InputState


type State = 
  { pointerPosition :: CoordinatePair
  , inputState :: InputState
  }

newtype Config = Config Keymap

keySignal :: Keycode -> Effect (Signal Keycode)
keySignal key = do
  signal <- keyPressed true key 
  let isKeyDown = eq true
  let isKeyUp = not <<< isKeyDown
  let thisKeyFilter = if supportsKeyUp key then isKeyUp else isKeyDown
  let default = if supportsKeyUp key then false else true  -- that is the way it has to be
  pure $ const key <$> filter thisKeyFilter default signal

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
    processSignal' (KeyPressSignal key) state@{ inputState: NoInputYet } = do 
       Lichess.dimHighlights
       possibleMoves <- join <<< Unfoldable.fromMaybe <$> (runMaybeT $ do
         piece <- MaybeT $ pure $ keyToPiece key
         square <- Lichess.coordsToSquare state.pointerPosition
         Lichess.findPossibleMoves piece square
         ) :: Effect (Array PieceOnBoard)
       handlePossibleMoves state possibleMoves
    -- TODO: reset disambiguation using ESC (customizable)
    processSignal' (KeyPressSignal key) state@{ inputState: DisambiguationNeeded possibleMoves } = do
       Lichess.dimHighlights
       newPossibleMoves <- fromMaybe possibleMoves <$> (runMaybeT $ do
          dir <- MaybeT $ pure $ keyToDisambiguation key
          orientation <- Lichess.getOrientation
          pure $ filterByDirection orientation dir possibleMoves)
       handlePossibleMoves state newPossibleMoves
    processSignal' (MovePointer coords) s = do
       pure $ s { pointerPosition = coords }

    handlePossibleMoves :: State -> Array PieceOnBoard -> Effect State
    handlePossibleMoves state [] = pure state
    handlePossibleMoves state [(PieceOnBoard _ source)] = do
       _ <- runMaybeT $ do
         dest <- Lichess.coordsToSquare state.pointerPosition
         lift $ movePiece source dest
       pure state { inputState = NoInputYet }
    handlePossibleMoves state moves = do
      Lichess.highlightSquares moves
      pure $ state { inputState = DisambiguationNeeded moves }

   in processSignal'

movePiece :: Square -> Square -> Effect Unit
movePiece from to = do
  _ <- runMaybeT $ Lichess.makeAMove from to
  pure unit

main :: Effect Unit
main = do
  let keymap = defaultKeymap
  let initialState = { pointerPosition: {x: 0, y: 0}
                     , inputState: NoInputYet
                     }
  keymapSignals' <- keymapSignals keymap
  movePointerSignal' <- mousePos
  let sig = (MovePointer <$> movePointerSignal')
         <> (KeyPressSignal <$> keymapSignals')
  _ <- foldEffect (processSignal keymap) initialState sig :: Effect (Signal State)
  Lichess.enablePlugin

