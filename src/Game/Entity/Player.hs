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
import GHC.Generics (Generic)
import Data.Generics.Sum
import Data.Glyph
import Data.Hitpoints
import Data.Position
import Optics hiding (Is)
import Optics.Operators.Unsafe
import Optics.Tupled

data Self = Self

data Player = Player Self Position Glyph Color HP deriving Generic

instance Tupled Player (Self, Position, Glyph, Color, HP) where
  tupled = iso (^?! _Ctor @"Player") (_Ctor @"Player" #)

initial :: Player
initial = tupled # (Self, (Position 3), (Glyph '@'), White, (HP 100 100))
