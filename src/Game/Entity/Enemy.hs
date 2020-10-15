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

module Game.Entity.Enemy
  ( Enemy (Enemy),
    Impl,
    initial,
  )
where

import Data.Amount
import Data.Color
import Data.Experience
import Data.Generics.Sum
import Data.Glyph
import Data.Name (Name)
import Dhall
import Game.Callbacks qualified as Callbacks
import Optics
import Optics.Operators.Unsafe
import Optics.Tupled

type Impl = (Name, Glyph, Color, Callbacks.Collision, Amount, XP)

data Enemy = Enemy
  { name :: Name,
    glyph :: Glyph,
    color :: Color,
    behavior :: Callbacks.Collision,
    canDrop :: Amount,
    yieldsXP :: XP
  }
  deriving stock (Generic)
  deriving anyclass (FromDhall)

instance Tupled Enemy Impl where
  tupled = iso (^?! _Ctor @"Enemy") (_Ctor @"Enemy" #)

initial :: Enemy
initial = tupled # ("gibbering idiot", Glyph '?', Yellow, Callbacks.Attack, Amount 0, XP 5 0)
