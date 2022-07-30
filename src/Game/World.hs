{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The data structures generated by the Apecs ECS to hold
-- relevant game state. Unfortunately, because Apecs uses IORef
-- values for speed, we can't serialize 'World' structures directly
-- to disk.
module Game.World (module Game.World) where

import Apecs
import Control.Algebra qualified as Eff
import Control.Carrier.Reader qualified as Eff
import Data.Amount
import Data.Experience
import Data.Glyph
import Data.Hitpoints
import Game.Entity.Inventory
import Game.Entity.Enemy qualified as Enemy (Tag)
import Game.Entity.Terrain qualified as Terrain (Tag)
import Data.Name (Name)
import Game.Flag qualified as Flag
import Data.Position
import GHC.Generics (Generic)
import Raw.Types
import Raw.Id qualified as Raw

makeWorld "World"
  [ ''Color,
    ''Flag.Persist,
    ''Glyph,
    ''HP,
    ''XP,
    ''Name,
    ''Position,
    ''Raw.Id,

    -- relations
    ''Inventory,

    -- behaviors
    ''Collision,
    ''Strategy,
    ''Flag.Impassable,

    -- numeric quantiites
    ''Amount,
    ''Hearing,

    -- tags
    ''Enemy.Tag,
    ''Terrain.Tag,
    ''Flag.Dirty
  ]

type WorldT = Apecs.SystemT World

pattern VERSION :: Int
pattern VERSION = 1

deriving stock instance Generic World

deriving newtype instance
  Eff.Algebra sig m =>
  Eff.Algebra (Eff.Reader World Eff.:+: sig) (SystemT World m)
