{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Game.Entity.Enemy
  ( Tag (..),
    Enemy (..),
    _Enemy,
    fromRaw,
  )
where

import Apecs (Component (..), Map)
import Data.Amount
import Data.Experience (XP (XP))
import Data.Glyph (Glyph (Glyph))
import Data.Hitpoints
import Data.Monoid
import Data.Name
import Data.Position (Position, HasPosition (..))
import Data.Store.Exts (Store)
import Data.Text qualified as Text
import GHC.Generics hiding (to)
import Optics
import Raw.Types (Color)
import Raw.Types qualified as Raw
import Apecs.Exts (Unique)

data Tag = EnemyTag
  deriving stock (Generic)
  deriving anyclass (Store)

instance Semigroup Tag where _ <> _ = EnemyTag
instance Monoid Tag where mempty = EnemyTag

data Enemy = Enemy
  { enemyTag :: Tag,
    enemyName :: Name,
    enemyColor :: Color,
    enemyGlyph :: Glyph,
    enemyId :: Raw.Id,
    enemyPosition :: Position,
    enemyHearing :: Hearing,
    enemyCollision :: Raw.Collision,
    enemyStrategy :: Raw.Strategy,
    enemyHP :: HP,
    enemyGold :: Amount,
    enemyXP :: XP
  }
  deriving stock Generic
  deriving anyclass (HasPosition, HasName, Store)

makeFieldLabels ''Enemy
makePrisms ''Enemy

instance Apecs.Component Tag where type Storage Tag = Unique Tag

fromRaw :: Position -> Raw.Id -> Raw.Enemy -> Enemy
fromRaw p ident e =
  Enemy
    EnemyTag
    (e ^. #name & Name)
    (e ^. #color)
    (e ^. #glyph & Text.head & Glyph)
    ident
    p
    (e ^. #hearing & Hearing)
    (e ^. #onCollide)
    (e ^. #strategy)
    (HP 5 5)
    (e ^. #canDrop & Amount)
    (e ^. #yieldsXP % to Sum & flip XP 100)
