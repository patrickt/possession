{-# LANGUAGE ImportQualifiedPost #-}

module Game.Entity.Enemy
  ( Tag (..),
    Enemy,
    fromRaw,
  )
where

import Apecs (Component (..), Map)
import Data.Amount
import Data.Experience (XP (XP))
import Data.Glyph (Glyph (Glyph))
import Data.Hitpoints
import Data.Monoid
import Data.Name (Name (Name))
import Data.Position (Position)
import Data.Store.Exts (Store)
import Data.Text qualified as Text
import GHC.Generics hiding (to)
import Optics
import Raw.Types (Color)
import Raw.Types qualified as Raw

data Tag = Enemy
  deriving stock (Generic)
  deriving anyclass (Store)

instance Apecs.Component Tag where type Storage Tag = Map Tag

type Enemy =
  ( (Tag, Name, Color, Glyph, Raw.Id, Position, Hearing),
    (Raw.Collision, Raw.Strategy, HP, Amount, XP)
  )

fromRaw :: Position -> Raw.Id -> Raw.Enemy -> Enemy
fromRaw p ident e =
  ( ( Enemy,
      e ^. #name & Name,
      e ^. #color,
      e ^. #glyph & Text.head & Glyph,
      ident,
      p,
      e ^. #hearing & Hearing
    ),
    ( e ^. #onCollide,
      e ^. #strategy,
      HP 5 5,
      e ^. #canDrop & Amount,
      e ^. #yieldsXP % to Sum & flip XP 100
    )
  )
