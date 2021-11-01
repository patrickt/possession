{-# LANGUAGE ImportQualifiedPost #-}

module Game.Entity.Enemy
  ( Tag (..)
  , Enemy
  , fromRaw
  )
  where

import Data.Amount (Amount (Amount))
import Data.Experience (XP (XP))
import Data.Name (Name (Name))
import Raw.Types qualified as Raw
import Optics
import Data.Hitpoints
import Data.Position (Position)
import Data.Store.Exts (Store)
import GHC.Generics hiding (to)
import Apecs (Component (..), Map)
import Data.Monoid
import Raw.Types (Color)
import qualified Data.Text as Text
import Data.Glyph (Glyph(Glyph))

data Tag = Enemy
  deriving stock Generic
  deriving anyclass Store

instance Apecs.Component Tag where type Storage Tag = Map Tag

type Enemy = ((Tag, Name, Color, Glyph, Raw.Id, Position), (Raw.Collision, HP, Amount, XP))


fromRaw :: Position -> Raw.Id -> Raw.Enemy -> Enemy
fromRaw p ident e =
  ( (Enemy,
    e ^. #name & Name,
    e ^. #color,
    e ^. #glyph & Text.head & Glyph,
    ident,
    p),
    (e ^. #onCollide,
    HP 5 5,
    e ^. #canDrop & Amount,
    e ^. #yieldsXP % to Sum & flip XP 100)
  )
