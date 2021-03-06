{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Operations on the player.
module Game.Entity.Player where

import Data.Amount (Amount)
import Data.Color
import Data.Experience
import Data.Generics.Sum
import Data.Glyph
import Data.Hitpoints
import Data.Position
import GHC.Generics (Generic)
import Optics hiding (Is)
import Optics.Operators.Unsafe
import Optics.Tupled

type Wealth = Amount

-- This kind of C++-ish pattern is used to cope with the fact
-- that Apecs deals in tuples exclusively (which is useful!
-- but not as typesafe as we want).
type Impl = (Position, Glyph, Color, HP, Wealth, XP)

data Player = Player
  { position :: Position,
    glyph :: Glyph,
    color :: Color,
    hp :: HP,
    wealth :: Wealth,
    experience :: XP
  }
  deriving (Generic)

instance Tupled Player Impl where
  tupled = iso (^?! _Ctor @"Player") (_Ctor @"Player" #)

initial :: Player
initial = tupled # (Position 3, Glyph '@', White, HP 100 100, 0 :: Amount, XP 0 100)
