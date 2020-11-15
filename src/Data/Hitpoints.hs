-- | A (ℕ, ℕ) tuple representing the health of living things.
module Data.Hitpoints (HP (..)) where

import Numeric.Natural

data HP = HP {current :: !Natural, total :: !Natural}
  deriving Show
