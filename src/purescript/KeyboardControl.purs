module KeyboardControl where

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
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import OCES.Chess (PieceOnBoard(..), Square)
import OCES.Disambiguation (filterByDirection)
import OCES.Keyboard (Keycode, supportsKeyDown, supportsKeyUp)
import OCES.Lichess as Lichess
import OCSE.KeyboardControl.Keymap (Keymap, defaultKeymap)
import Signal (Signal, constant, filter, mergeMany)
import Signal.DOM (mousePos, CoordinatePair)
import Signal.DOM.Prevented (keyPressed)
import Signal.Effect (foldEffect)
import WebExtension.Storage as Storage

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

type Config = 
  { keymap :: Keymap
  , shouldTolerateMissclicks :: Boolean
  , preferredKeyEventType :: KeyEventType
  }

data KeyEventType = KeyUp | KeyDown
derive instance eqKeyEvent :: Eq KeyEventType

keySignal :: KeyEventType -> Keycode -> Effect (Signal Keycode)
keySignal preferredKeyEventType key = 
  let 
    isKeyDown = eq true
    isKeyUp = not <<< isKeyDown
    pref = if preferredKeyEventType == KeyUp 
             then {supported: supportsKeyUp, eventFilter: {primary: isKeyUp, secondary: isKeyDown}, default: false} 
             else {supported: supportsKeyDown, eventFilter: {primary: isKeyDown, secondary: isKeyUp}, default: true}
  in do
    signal <- keyPressed true key 
    let thisKeyFilter = if pref.supported key then pref.eventFilter.primary else pref.eventFilter.secondary
    pure $ const key <$> filter thisKeyFilter pref.default signal

keymapSignals :: KeyEventType -> Keymap -> Effect (Signal Keycode)
keymapSignals preferredKeyEventType keymap = 
  let
      pieceKeys = keymap.pieceKey <$> upFromIncluding bottom
      disambiguationKeys = keymap.disambiguationKey <$> upFromIncluding bottom
      allKeys = nub $ pieceKeys <> disambiguationKeys <> [keymap.cancelKey]
      allSignals = traverse (keySignal preferredKeyEventType) allKeys
  in do
     mergedSignals <- mergeMany <$> allSignals
     pure $ fromMaybe (constant (-1)) mergedSignals


data AppSignal 
  = KeyPressSignal Keycode
  | MovePointer CoordinatePair

processSignal :: Config -> AppSignal -> State -> Effect State
processSignal config = 
  let 
    keyToPiece = reverseMap config.keymap.pieceKey
    keyToDisambiguation = reverseMap config.keymap.disambiguationKey

    processSignal' :: AppSignal -> State -> Effect State
    processSignal' (KeyPressSignal key) state | key == config.keymap.cancelKey = do 
       Lichess.dimHighlights
       pure state { inputState = NoInputYet }
    processSignal' (KeyPressSignal key) state@{ inputState: NoInputYet } = do 
       Lichess.dimHighlights
       possibleMoves <- join <<< Unfoldable.fromMaybe <$> (runMaybeT $ do
         piece <- MaybeT $ pure $ keyToPiece key
         square <- Lichess.coordsToSquare state.pointerPosition
         pieceMoves <- Lichess.findPossibleMoves piece square
         if pieceMoves == [] && config.shouldTolerateMissclicks then
          Lichess.findAllPossibleMoves square
         else
           pure pieceMoves
         ) :: Effect (Array PieceOnBoard)
       handlePossibleMoves state possibleMoves
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
main = launchAff_ mainAff

mainAff :: Aff Unit
mainAff = do
  let keymap = defaultKeymap
  let config = { keymap: keymap 
               , shouldTolerateMissclicks: true 
               , preferredKeyEventType: KeyDown
               }
  liftEffect $ runKeyboardControl config

runKeyboardControl :: Config -> Effect Unit
runKeyboardControl config = do
  let initialState = { pointerPosition: {x: 0, y: 0}
                     , inputState: NoInputYet
                     }
  keymapSignals' <- keymapSignals config.preferredKeyEventType config.keymap
  movePointerSignal' <- mousePos
  let sig = (MovePointer <$> movePointerSignal')
         <> (KeyPressSignal <$> keymapSignals')
  _ <- foldEffect (processSignal config) initialState sig :: Effect (Signal State)
  Lichess.enablePlugin

