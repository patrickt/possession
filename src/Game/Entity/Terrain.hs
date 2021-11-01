{-# LANGUAGE ImportQualifiedPost #-}

module Game.Entity.Terrain
  (Tag (..),
   Terrain,
   wall
  ) where

import Raw.Types qualified as Color (Color (..))
import Data.Glyph (Glyph (..))
import Data.Position (Position)
import qualified Raw.Types as Raw
import qualified Game.Flag as Flag
import GHC.Generics
import Data.Store.Exts (Store)
import Apecs (Component (..), Map)

data Tag = Wall
  deriving stock Generic
  deriving anyclass Store

instance Apecs.Component Tag where type Storage Tag = Map Tag

type Terrain = (Tag, Position, Glyph, Raw.Color, Raw.Collision, Flag.Persist)

wall :: Position -> Terrain
wall p = (Wall, p, Glyph '#', Color.White, Raw.Invalid, Flag.Persist)
