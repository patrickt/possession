{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Game.Info (module Game.Info) where

import Data.Monoid
import Data.Monoid.Generic
import Data.Hitpoints (HP)
import Data.Generics.Product
import Optics
import GHC.Generics (Generic)

data Info = Info
  { playerHitpoints :: Last HP
  }
  deriving stock (Generic)
  deriving (Semigroup) via GenericSemigroup Info
  deriving (Monoid) via GenericMonoid Info

hitpoints :: Lens' Info (Maybe HP)
hitpoints = field @"playerHitpoints" %coerced
