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
import Optics hiding (Is)
import Optics.Tupled

data Self = Self

newtype Player = Player
  ( Self
  , Position
  , Glyph
  , Color
  , HP
  )

instance Tupled Player (Self, Position, Glyph, Color, HP) where
  tupled = coerced

initial :: Player
initial = Player (Self, (Position 3), (Glyph '@'), White, (HP 100 100))
