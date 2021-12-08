{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | A rendering-agnostic container for information about the state of the world.
-- This is created in the ECS loop and sent via a broker to the rendering engine.
module Game.Info
  ( Info (Info),
    HasInfo (..),
    position,
    allEnemies,
    gold,
    summary,
    enemyAt,
    hitpoints,
    xp,
  atlas)
where

import Data.Amount
import Data.Experience (XP)
import Data.Generics.Product hiding (position)
import Data.Hitpoints (HP (HP))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid
import Data.Monoid.Generic
import Data.Name (Name, name)
import Data.Position hiding (position)
import Data.Position qualified as P
import GHC.Generics (Generic)
import Game.Entity.Enemy (Enemy)
import Optics
import Apecs.Exts (Entity)

data Info = Info
  { infoHitpoints :: Last HP,
    infoGold :: Sum Amount,
    infoXp :: XP,
    infoPosition :: Last Position,
    infoSummary :: Dual (Map Position Enemy),
    infoAtlas :: Dual (Map Position Entity)
  }
  deriving stock (Generic)
  deriving (Semigroup) via GenericSemigroup Info
  deriving (Monoid) via GenericMonoid Info

makeFieldLabels ''Info
makeClassy ''Info

hitpoints :: HasInfo a => Lens' a HP
hitpoints = info % #hitpoints % coerced % non (HP 0 0)

xp :: HasInfo a => Lens' a XP
xp = info % #xp

position :: HasInfo a => AffineTraversal' a Position
position = singular (info % #position % traversed)

gold :: HasInfo a => Lens' a Amount
gold = info % #gold % coerced

allEnemies :: Info -> [(Position, Name)]
allEnemies = fmap (\e -> (e ^. P.position, e ^. name)) . Map.elems . view summary

enemyAt :: HasInfo t => Position -> AffineTraversal' t Enemy
enemyAt p = info % summary % ix p

summary :: Lens' Info (Map Position Enemy)
summary = #summary % coerced

atlas :: Lens' Info (Map Position Entity)
atlas = #atlas % coerced
