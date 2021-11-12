module OCSE.KeyboardControl.Keymap where

import Prelude

import Data.Array (filter, find, zip)
import Data.Array.Partial as PartialArray
import Data.Either (hush, note)
import Data.Either as Either
import Data.Enum (class Enum, upFromIncluding)
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd)
import Effect.Aff (Aff, attempt)
import OCES.Chess (Piece(..))
import OCES.Disambiguation (DisambiguationDirection(..))
import OCES.Keyboard (Keycode, escKey, keycodeFor)
import Partial.Unsafe (unsafePartial)
import Simple.JSON (readJSON, writeJSON)
import WebExtension.Storage as Storage

type Keymap =
  { pieceKey :: Piece -> Keycode
  , disambiguationKey :: DisambiguationDirection -> Keycode
  , cancelKey :: Keycode
  }


defaultKeymap :: Keymap
defaultKeymap =
  let pieceKey Pawn = keycodeFor 'w' 
      pieceKey Rook = keycodeFor 'a' 
      pieceKey Knight = keycodeFor 's' 
      pieceKey Bishop = keycodeFor 'd' 
      pieceKey King = keycodeFor 'e'
      pieceKey Queen = keycodeFor 'q'
      disambiguationKey Top = keycodeFor 'w'
      disambiguationKey Bottom = keycodeFor 's'
      disambiguationKey Left = keycodeFor 'a'
      disambiguationKey Right = keycodeFor 'd'
   in
  { pieceKey: pieceKey
  , disambiguationKey : disambiguationKey
  , cancelKey : escKey
  }


keymapPrefKey :: String
keymapPrefKey = "keymap"

loadKeymap :: Aff Keymap
loadKeymap = do
  eitherKeymap <- attempt (Storage.get Storage.Local keymapPrefKey) <#> 
    note "couldn't read from storage" <<< hush <#>
    \kmap -> kmap >>= jsonDecodeKeymap
  pure $ fromMaybe defaultKeymap (hush eitherKeymap)


type EncodedFunction k v = Array { key :: k, value :: v }

type IntermediateKeymapRep = 
  { pieces :: EncodedFunction String Keycode
  , disambiguation :: EncodedFunction String Keycode
  , cancel :: Keycode
  }


jsonEncodeKeymap :: Keymap -> String
jsonEncodeKeymap = encodeKeymap >>> writeJSON

encodeKeymap :: Keymap -> IntermediateKeymapRep
encodeKeymap keymap = 
  let pieces = enumFunctionToRecord keymap.pieceKey
      disambiguation = enumFunctionToRecord keymap.disambiguationKey
      cancel = keymap.cancelKey
   in 
   { "pieces": pieces 
   , "disambiguation": disambiguation 
   , "cancel": cancel 
   }

allValues :: forall a. Enum a => Bounded a => Array a
allValues = upFromIncluding bottom

enumFunctionToRecord :: forall e x. Show e => Enum e => Bounded e => (e -> x) -> EncodedFunction String x
enumFunctionToRecord f = (\k -> {key: show k, value: f k}) <$> allValues

joinMaybeFun :: forall a e. Partial => Enum e => Bounded e => (e -> Maybe a) -> Maybe (e -> a)
joinMaybeFun f = do
  results <- traverse f allValues
  let resultsMap = zip allValues results :: Array (Tuple e a)
  pure $ (snd <<< PartialArray.head <$> \e -> filter (\(Tuple k _) -> k == e) resultsMap)

decodeEnumFunction :: forall e x. Show e => Enum e => Bounded e => EncodedFunction String x -> Either.Either String (e -> x)
decodeEnumFunction ef = 
  let
    f = (\entity -> find (\item -> item.key == show entity) ef <#> (_.value)) :: e -> Maybe x
    g = unsafePartial $ joinMaybeFun f :: Maybe (e -> x)
  in Either.note "couldn't decode function" g

jsonDecodeIntermediateKeymap :: String -> Either.Either String IntermediateKeymapRep
jsonDecodeIntermediateKeymap keymapStr = case readJSON keymapStr of
  Either.Right a -> Either.Right $ a
  Either.Left e -> Either.Left $ show e

decodeKeymap :: IntermediateKeymapRep -> Either.Either String Keymap
decodeKeymap ikm = do
     disambiguationKey <- decodeEnumFunction ikm.disambiguation
     pieceKey <- decodeEnumFunction ikm.pieces
     Either.Right { cancelKey: ikm.cancel 
                  , disambiguationKey: disambiguationKey 
                  , pieceKey: pieceKey 
                  }

jsonDecodeKeymap :: String -> Either.Either String Keymap
jsonDecodeKeymap json = jsonDecodeIntermediateKeymap json >>= decodeKeymap
