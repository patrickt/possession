{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.Entity.Enemy
  ( Enemy (Enemy),
    Impl,
    initial,
  )
where

import Data.Color
import Data.Generics.Sum
import Data.Glyph
import Data.Hitpoints
import Data.Name (Name)
import Data.Position (Position (..))
import Data.Text (Text)
import Game.Callbacks qualified as Callbacks
import Optics
import Optics.Operators.Unsafe
import Optics.Tupled
import GHC.Generics (Generic)

type Impl = (Name, Glyph, Color, Callbacks.Collision)

data Enemy = Enemy
  { name :: Name,
    glyph :: Glyph,
    color :: Color,
    behavior :: Callbacks.Collision
  } deriving Generic

instance Tupled Enemy Impl where
  tupled = iso (^?! _Ctor @"Enemy") (_Ctor @"Enemy" #)

initial :: Enemy
initial = tupled # ("gibbering idiot", Glyph '?', Yellow, Callbacks.Attack)
