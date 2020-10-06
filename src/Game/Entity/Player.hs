{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Game.Entity.Player where

import Data.Color
import Data.Generics.Sum
import Data.Glyph
import Data.Hitpoints
import Data.Is
import Data.Position
import GHC.Generics (Generic)
import Optics hiding (Is)

data Self = Self

data Player = Player
  { _self :: Self,
    _pos :: Position,
    _glyph :: Glyph,
    _color :: Color,
    _hp :: HP
  }
  deriving (Generic)

initial :: Player
initial = Player Self (Position 3) (Glyph '@') White (HP 100 100)

_Player :: Prism' Player (Self, Position, Glyph, Color, HP)
_Player = _Ctor @"Player"
