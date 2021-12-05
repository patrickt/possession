{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | A rendering-agnostic container for information about the state of the world.
-- This is created in the ECS loop and sent via a broker to the rendering engine.
module Game.Info
  ( Info (Info),
    HasInfo (..)
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
import Data.Map.Strict (Map)
import Game.Entity.Enemy (Enemy)
import Data.Generics.Product

data Info = Info
  { hitpoints :: Last HP,
    gold :: Sum Amount,
    xp :: XP,
    position :: Last Position,
    summary :: Map Position Enemy
  }
  deriving stock (Generic)
  deriving (Semigroup) via GenericSemigroup Info
  deriving (Monoid) via GenericMonoid Info

makeFieldLabelsWith noPrefixFieldLabels ''Info
makeClassy ''Info

instance Data.Position.HasPosition Info where
  position = #position % coerced % non (0 :: Position)
