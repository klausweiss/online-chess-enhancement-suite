module OCES.Missclicks where

import Prelude


data MissclickBehavior
  = IgnoreMissclicks
  | AlwaysAskForConfirmation
  | PerformOnlyMoves
derive instance eqMissclickBehavior :: Eq MissclickBehavior

toleratesMissclicks :: MissclickBehavior -> Boolean
toleratesMissclicks IgnoreMissclicks = false
toleratesMissclicks _ = true

