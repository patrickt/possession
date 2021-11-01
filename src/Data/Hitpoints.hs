-- | A (ℕ, ℕ) tuple representing the health of living things.
module Data.Hitpoints (HP (..), injure, isDead) where

import Data.Store (Store)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Apecs (Component (..), Map)

data HP = HP {current :: !Integer, total :: !Natural}
  deriving stock (Show, Generic)
  deriving anyclass (Store)

instance Apecs.Component HP where type Storage HP = Map HP

injure :: Integral a => a -> HP -> HP
injure n (HP c t) = HP (c - fromIntegral n) t

isAlive, isDead :: HP -> Bool
isAlive (HP x _) = x >= 0
isDead = not . isAlive
