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
import Data.Amount (Amount)

type Wealth = Amount

data Player = Player Position Glyph Color HP Wealth  deriving Generic

instance Tupled Player (Position, Glyph, Color, HP, Wealth) where
  tupled = iso (^?! _Ctor @"Player") (_Ctor @"Player" #)

initial :: Player
initial = tupled # ((Position 3), (Glyph '@'), White, (HP 100 100), 0 :: Amount)
