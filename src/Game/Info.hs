{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

module Game.Info (module Game.Info) where

import Data.Generics.Product
import Data.Hitpoints (HP)
import Data.Monoid
import Data.Monoid.Generic
import GHC.Generics (Generic)
import Optics

data Info = Info
  { playerHitpoints :: Last HP
  }
  deriving stock (Generic)
  deriving (Semigroup) via GenericSemigroup Info
  deriving (Monoid) via GenericMonoid Info

hitpoints :: Lens' Info (Maybe HP)
hitpoints = field @"playerHitpoints" % coerced
