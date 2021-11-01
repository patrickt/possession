-- | A (ℕ, ℕ) tuple representing the health of living things.
module Data.Hitpoints
  ( HP (..),
    injure,
    isDead,
    isAlive,
  )
where

import Apecs (Component (..), Map)
import Data.Store (Store)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

data HP = HP {current :: !Integer, total :: !Natural}
  deriving stock (Show, Generic)
  deriving anyclass (Store)

instance Apecs.Component HP where type Storage HP = Map HP

injure :: Integral a => a -> HP -> HP
injure n (HP c t) = HP (c - fromIntegral n) t

isAlive, isDead :: HP -> Bool
isAlive (HP x _) = x >= 0
isDead = not . isAlive
