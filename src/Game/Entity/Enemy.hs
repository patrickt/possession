{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.Entity.Enemy
  ( Enemy (Enemy),
    Impl,
    initial,
  )
where

import Data.Glyph
import Data.Hitpoints
import Data.Color
import Data.Position (Position (..))
import Data.Text (Text)
import Game.Callbacks qualified as Callbacks
import Optics
import Optics.Tupled

type Impl = (Text, HP, Glyph, Color, Position, Callbacks.Collision)

newtype Enemy = Enemy Impl

instance Tupled Enemy Impl where
  tupled = coerced

initial :: Enemy
initial = Enemy ("gibbering idiot", HP 0 0, Glyph '?', Yellow, Position 6, Callbacks.Attack)
