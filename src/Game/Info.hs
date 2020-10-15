{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

-- | A rendering-agnostic container for information about the state of the world.
-- This is created in the ECS loop and sent via a broker to the rendering engine.
module Game.Info
  ( Info (Info)
  , hitpoints
  , gold
  ) where

import Data.Generics.Product
import Data.Hitpoints (HP)
import Data.Monoid
import Data.Monoid.Generic
import GHC.Generics (Generic)
import Optics
import Data.Amount

data Info = Info
  { playerHitpoints :: Last HP
  , playerGold :: Sum Amount
  }
  deriving stock (Generic)
  deriving (Semigroup) via GenericSemigroup Info
  deriving (Monoid) via GenericMonoid Info

hitpoints :: Lens' Info (Maybe HP)
hitpoints = field @"playerHitpoints" % coerced

gold :: Lens' Info Amount
gold = field @"playerGold" % coerced
