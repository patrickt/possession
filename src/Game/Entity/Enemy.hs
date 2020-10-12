{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Game.Entity.Enemy
  ( Enemy (Enemy),
    Self (..),
    Impl,
    initial,
  )
where

import Data.Generics.Sum
import Data.Glyph
import Data.Hitpoints
import Data.Color
import Data.Position (Position (..))
import Data.Text (Text)
import Game.Callbacks qualified as Callbacks
import GHC.Generics (Generic)
import Optics
import Optics.Tupled

data Self = Self

type Impl = (Self, Text, HP, Glyph, Color, Position, Callbacks.Collision)

newtype Enemy = Enemy Impl

instance Tupled Enemy Impl where
  tupled = coerced

initial :: Enemy
initial = Enemy (Self, "gibbering idiot", HP 0 0, Glyph '?', Yellow, Position 6, Callbacks.Attack)
