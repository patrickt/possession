{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Game.Entity.Player where

import Data.Color
import Data.Glyph
import Data.Hitpoints
import Data.Position
import GHC.Generics (Generic)
import Optics hiding (Is)
import Optics.Tupled

data Self = Self

data Player = Player
  { _self :: Self,
    _pos :: Position,
    _glyph :: Glyph,
    _color :: Color,
    _hp :: HP
  }
  deriving (Generic)

instance Tupled Player (Self, Position, Glyph, Color, HP) where
  tupled = iso (\Player{..} -> (_self, _pos, _glyph, _color, _hp)) (\(_self, _pos, _glyph, _color, _hp) -> Player{..})

initial :: Player
initial = Player Self (Position 3) (Glyph '@') White (HP 100 100)
