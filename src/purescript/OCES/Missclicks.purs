module OCES.Missclicks where

import Prelude

import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import WebExtension.Storage as Storage


data MissclickBehavior
  = IgnoreMissclicks
  | AlwaysAskForConfirmation
  | PerformOnlyMoves
derive instance eqMissclickBehavior :: Eq MissclickBehavior
derive instance genericDisambiguationDirection :: Generic MissclickBehavior _
instance showDisambiguationDirection :: Show MissclickBehavior where
  show = genericShow

parseMissclickBehavior :: String -> Maybe MissclickBehavior
parseMissclickBehavior b 
  | b == show IgnoreMissclicks = Just IgnoreMissclicks
  | b == show AlwaysAskForConfirmation = Just AlwaysAskForConfirmation
  | b == show PerformOnlyMoves = Just PerformOnlyMoves
parseMissclickBehavior _ = Nothing

defaultMissclickBehavior :: MissclickBehavior
defaultMissclickBehavior = AlwaysAskForConfirmation

toleratesMissclicks :: MissclickBehavior -> Boolean
toleratesMissclicks IgnoreMissclicks = false
toleratesMissclicks _ = true

missclickBehaviorPrefKey :: String
missclickBehaviorPrefKey = "missclickBehavior"

saveMissclickBehavior :: MissclickBehavior -> Aff Unit
saveMissclickBehavior mb = do
  void $ attempt (Storage.set Storage.Local missclickBehaviorPrefKey (show mb))

loadMissclickBehavior :: Aff MissclickBehavior
loadMissclickBehavior = do
  maybeMissclickBehavior <- 
    attempt (Storage.get Storage.Local missclickBehaviorPrefKey) 
    <#> hush 
    <#> \mb -> mb >>= parseMissclickBehavior
  pure $ fromMaybe defaultMissclickBehavior maybeMissclickBehavior

