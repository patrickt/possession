{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Game.Entity.Player where

import Data.Is
import Optics hiding (Is)
import Data.Glyph
import Data.Generics.Sum
import GHC.Generics (Generic)
import Data.Color
import Data.Hitpoints
import Data.Position

data Self = Self

data Player = Player
  { _self :: Self
  , _pos :: Position
  , _glyph :: Glyph
  , _color :: Color
  , _hp :: HP
  } deriving Generic

_Player :: Prism' Player (Self, Position, Glyph, Color, HP)
_Player = _Ctor @"Player"
