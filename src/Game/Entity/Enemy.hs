{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Operations and accessors on enemy values.
-- Should probably be called something else.
module Game.Entity.Enemy
  ( Enemy (Enemy),
    Impl,
  )
where

import Data.Amount
import Data.Color
import Data.Experience
import Data.Generics.Sum
import Data.Glyph
import Data.Name (Name)
import Dhall
import Game.Behavior qualified as Behavior
import Optics
import Optics.Operators.Unsafe
import Optics.Tupled

type Impl = (Name, Glyph, Color, Behavior.Collision, Amount, XP)

data Enemy = Enemy
  { name :: Name,
    glyph :: Glyph,
    color :: Color,
    behavior :: Behavior.Collision,
    canDrop :: Amount,
    yieldsXP :: XP
  }
  deriving stock (Generic)
  deriving anyclass (FromDhall)

instance Tupled Enemy Impl where
  tupled = iso (^?! _Ctor @"Enemy") (_Ctor @"Enemy" #)
