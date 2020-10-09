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

data Enemy = Enemy
  { _self :: Self,
    _name :: Text,
    _hitpoints :: HP,
    _glyph :: Glyph,
    _color :: Color,
    _pos :: Position
  }
  deriving (Generic)

instance Tupled Enemy (Self, Text, HP, Glyph, Color, Position) where
  tupled = iso (\Enemy{..} -> (_self, _name, _hitpoints, _glyph, _color, _pos)) (\(_self, _name, _hitpoints, _glyph, _color, _pos) -> Enemy {..})

initial :: Enemy
initial = Enemy Self "" (HP 0 0) (Glyph '?') Yellow (Position 6)
