{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Operations on the player.
module Game.Entity.Player where

import Data.Amount (Amount)
import Data.Experience
import Data.Glyph
import Data.Hitpoints
import Data.Position
import Raw.Types

type Wealth = Amount

-- This kind of C++-ish pattern is used to cope with the fact
-- that Apecs deals in tuples exclusively (which is useful!
-- but not as typesafe as we want).
type Player = (Position, Glyph, Color, HP, Wealth, XP)

initial :: Player
initial = (3, Glyph '@', White, HP 100 100, 0 :: Amount, XP 0 100)
