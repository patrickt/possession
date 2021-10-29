{-# LANGUAGE ImportQualifiedPost #-}

module Game.Entity.Enemy where

import Data.Amount (Amount)
import Data.Experience (XP)
import Data.Name (Name)
import Game.Behavior qualified as Behavior
import Raw.Enemy qualified as Raw
import Optics
import Data.Hitpoints
import Data.Position (Position)
import Data.Store.Exts (Store)
import GHC.Generics

data Tag = Enemy
  deriving stock Generic
  deriving anyclass Store

type Enemy = (Tag, Name, Position, Behavior.Collision, HP, Amount, XP)

fromRaw :: Position -> Raw.Enemy -> Enemy
fromRaw p e =
  ( Enemy,
    e ^. #name,
    p,
    e ^. #behavior,
    HP 5 5,
    e ^. #canDrop,
    e ^. #yieldsXP
  )
