{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Operations and accessors on enemy values.
-- Should probably be called something else.
module Game.Entity.Enemy
  ( Enemy (Enemy),
  )
where

import Data.Amount
import Data.Color
import Data.Experience
import Data.Glyph
import Data.Name (Name)
import Dhall
import Game.Behavior qualified as Behavior
import Optics

data Enemy = Enemy
  { name :: Name,
    glyph :: Glyph,
    color :: Color,
    behavior :: Behavior.Collision,
    canDrop :: Amount,
    yieldsXP :: XP
  }
  deriving stock (Generic)
  deriving anyclass (FromDhall)

makeFieldLabelsNoPrefix ''Enemy
