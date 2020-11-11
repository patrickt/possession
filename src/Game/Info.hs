{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A rendering-agnostic container for information about the state of the world.
-- This is created in the ECS loop and sent via a broker to the rendering engine.
module Game.Info
  ( Info (Info),
  )
where

import Data.Amount
import Data.Experience (XP)
import Data.Hitpoints (HP)
import Data.Monoid
import Data.Monoid.Generic
import Data.Position
import GHC.Generics (Generic)
import Optics

data Info = Info
  { hitpoints :: Last HP,
    gold :: Sum Amount,
    xp :: XP,
    position :: Last Position
  }
  deriving stock (Generic)
  deriving (Semigroup) via GenericSemigroup Info
  deriving (Monoid) via GenericMonoid Info

makeFieldLabelsWith noPrefixFieldLabels ''Info
