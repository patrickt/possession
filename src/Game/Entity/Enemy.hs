{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Game.Entity.Enemy
  ( Enemy (Enemy),
    _Enemy,
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

data Self = Self

data Enemy = Enemy
  { _self :: Self,
    _name :: Text,
    _hitpoints :: HP,
    _glyph :: Glyph,
    _color :: Color,
    _pos :: Position,
    _behavior :: Callbacks.Callbacks Enemy
  }
  deriving (Generic)

initial :: Enemy
initial = Enemy Self "" (HP 0 0) (Glyph '?') Yellow (Position 6) Callbacks.hostile

_Enemy :: Prism' Enemy _
_Enemy = _Ctor @"Enemy"
