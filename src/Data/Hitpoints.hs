{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | A (ℕ, ℕ) tuple representing the health of living things.
module Data.Hitpoints (HP (..)) where

import Data.Store.Exts (Store)
import GHC.Generics (Generic)
import Numeric.Natural

data HP = HP {current :: !Natural, total :: !Natural}
  deriving stock (Show, Generic)
  deriving anyclass (Store)
