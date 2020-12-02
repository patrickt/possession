{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | A (ℕ, ℕ) tuple representing the health of living things.
module Data.Hitpoints (HP (..)) where

import GHC.Generics (Generic)
import Data.Store (Store)
import Numeric.Natural

data HP = HP {current :: !Natural, total :: !Natural}
  deriving stock (Show, Generic)
  deriving anyclass (Store)
