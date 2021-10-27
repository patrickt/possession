-- | A (ℕ, ℕ) tuple representing the health of living things.
module Data.Hitpoints (HP (..), injure, isDead) where

import Data.Store (Store)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

data HP = HP {current :: !Integer, total :: !Natural}
  deriving stock (Show, Generic)
  deriving anyclass (Store)

injure :: Integral a => a -> HP -> HP
injure n (HP c t) = HP (c - fromIntegral n) t

isDead :: HP -> Bool
isDead (HP x _) = x > 0
