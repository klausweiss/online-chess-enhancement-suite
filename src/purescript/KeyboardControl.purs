module KeyboardControl where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array (nub)
import Data.Enum (class Enum, upFromIncluding)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Unfoldable as Unfoldable
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import OCES.Chess (PieceOnBoard(..), Square)
import OCES.Disambiguation (filterByDirection)
import OCES.Keyboard (Keycode, supportsKeyDown, supportsKeyUp)
import OCES.Lichess as Lichess
import OCES.Missclicks (MissclickBehavior(..), toleratesMissclicks)
import OCSE.KeyboardControl.Keymap (Keymap, loadKeymap)
import Signal (Signal, constant, filter, mergeMany)
import Signal.DOM (mousePos, CoordinatePair)
import Signal.DOM.KeyEvent (KeyEvent, isKeyDown, isKeyUp, keyPressed, keycode, noopEvent, preventDefault)
import Signal.Effect (foldEffect)

reverseMap :: forall e k. Enum e => Bounded e => Ord k => (e -> k) -> k -> Maybe e
reverseMap enumToKey = 
  let
      allEnums = upFromIncluding bottom :: Array e
      keysMap = fromFoldable $ (\e -> Tuple (enumToKey e) e) <$> allEnums :: Map k e
   in
    (flip lookup) keysMap

data InputState
  = NoInputYet
  | DisambiguationNeeded
    { possibleMoves :: Array PieceOnBoard
    , forceDisambiguation :: Boolean
    }

derive instance eqInputState :: Eq InputState


type State = 
  { pointerPosition :: CoordinatePair
  , inputState :: InputState
  }

type Config = 
  { keymap :: Keymap
  , missclickBehavior :: MissclickBehavior
  , preferredKeyEventType :: KeyEventType
  }

data KeyEventType = KeyUp | KeyDown
derive instance eqKeyEvent :: Eq KeyEventType

keySignal :: KeyEventType -> Keycode -> Effect (Signal KeyEvent)
keySignal preferredKeyEventType key = 
  let 
    pref = if preferredKeyEventType == KeyUp 
             then { supported: supportsKeyUp
                  , eventFilter: {primary: isKeyUp, secondary: isKeyDown}
                  }
             else { supported: supportsKeyDown
                  , eventFilter: {primary: isKeyDown, secondary: isKeyUp}
                  }
  in do
    signal <- keyPressed key 
    let thisKeyFilter = if pref.supported key then pref.eventFilter.primary else pref.eventFilter.secondary
    pure $ filter thisKeyFilter noopEvent signal

keymapSignals :: KeyEventType -> Keymap -> Effect (Signal KeyEvent)
keymapSignals preferredKeyEventType keymap = 
  let
      pieceKeys = keymap.pieceKey <$> upFromIncluding bottom
      disambiguationKeys = keymap.disambiguationKey <$> upFromIncluding bottom
      allKeys = nub $ pieceKeys <> disambiguationKeys <> [keymap.cancelKey]
      allSignals = traverse (keySignal preferredKeyEventType) allKeys
  in do
     mergedSignals <- mergeMany <$> allSignals
     pure $ fromMaybe (constant noopEvent) mergedSignals


data AppSignal 
  = KeyPressSignal KeyEvent
  | MovePointer CoordinatePair

processSignal :: Config -> AppSignal -> State -> Effect State
processSignal config = 
  let 
    keyToPiece = reverseMap config.keymap.pieceKey
    keyToDisambiguation = reverseMap config.keymap.disambiguationKey

    listFromMaybeT mt = join <<< Unfoldable.fromMaybe <$> runMaybeT mt

    processSignal' :: AppSignal -> State -> Effect State
    processSignal' (KeyPressSignal keyEvent) state | keycode keyEvent == config.keymap.cancelKey = do 
       Lichess.dimHighlights
       preventDefault $ keyEvent
       pure state { inputState = NoInputYet }
    processSignal' (KeyPressSignal keyEvent) state@{ inputState: NoInputYet } = do 
       Lichess.dimHighlights
       let getSquare = Lichess.coordsToSquare state.pointerPosition
       pieceMoves <- listFromMaybeT do
         piece <- MaybeT <<< pure <<< keyToPiece <<< keycode $ keyEvent
         square <- getSquare
         pieceMoves <- Lichess.findPossibleMoves piece square
         preventDefault $ keyEvent
         pure pieceMoves
       let missclicked = pieceMoves == []
       possibleMoves <- 
         if missclicked && toleratesMissclicks config.missclickBehavior then
           listFromMaybeT do 
             square <- getSquare
             Lichess.findAllPossibleMoves square
         else
           pure pieceMoves
       let newState = (
           if missclicked && config.missclickBehavior == AlwaysAskForConfirmation then 
             state { inputState = DisambiguationNeeded { possibleMoves: possibleMoves, forceDisambiguation: true } }
           else 
             state
           )
       handlePossibleMoves newState possibleMoves
    processSignal' (KeyPressSignal keyEvent) state@{ inputState: DisambiguationNeeded disambiguationState } = do
       Lichess.dimHighlights
       newPossibleMoves <- fromMaybe disambiguationState.possibleMoves <$> (runMaybeT $ do
          dir <- MaybeT <<< pure <<< keyToDisambiguation <<< keycode $ keyEvent
          orientation <- Lichess.getOrientation
          preventDefault $ keyEvent
          pure $ filterByDirection orientation dir disambiguationState.possibleMoves)
       handlePossibleMoves state newPossibleMoves
    processSignal' (MovePointer coords) s = do
       pure $ s { pointerPosition = coords }

    handlePossibleMoves :: State -> Array PieceOnBoard -> Effect State
    handlePossibleMoves state [] = pure state
    handlePossibleMoves state@{ inputState: DisambiguationNeeded { forceDisambiguation: true } } moves 
      = showDisambiguation state moves
    handlePossibleMoves state [(PieceOnBoard _ source)] = do
       _ <- runMaybeT $ do
         dest <- Lichess.coordsToSquare state.pointerPosition
         movePiece source dest
       pure state { inputState = NoInputYet }
    handlePossibleMoves state moves 
      = showDisambiguation state moves

    showDisambiguation state moves = do
      Lichess.highlightSquares moves
      pure $ state { inputState = DisambiguationNeeded { possibleMoves: moves, forceDisambiguation: false } }

   in processSignal'

movePiece :: forall eff. MonadEffect eff => Square -> Square -> eff Unit
movePiece from to = liftEffect do
  void <<< runMaybeT $ Lichess.makeAMove from to

main :: Effect Unit
main = launchAff_ mainAff

mainAff :: Aff Unit
mainAff = do
  Lichess.enablePlugin
  keymap <- loadKeymap
  let config = { keymap: keymap 
               , missclickBehavior: AlwaysAskForConfirmation
               , preferredKeyEventType: KeyDown
               }
  listenToEvents config

listenToEvents :: forall eff. MonadEffect eff => Config -> eff Unit
listenToEvents config = liftEffect $ do
  let initialState = { pointerPosition: {x: 0, y: 0}
                     , inputState: NoInputYet
                     }
  keymapSignals' <- keymapSignals config.preferredKeyEventType config.keymap
  movePointerSignal' <- mousePos
  let sig = (MovePointer <$> movePointerSignal')
         <> (KeyPressSignal <$> keymapSignals')
  _ <- foldEffect (processSignal config) initialState sig :: Effect (Signal State)
  pure unit
