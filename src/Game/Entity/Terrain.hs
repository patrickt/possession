{-# LANGUAGE ImportQualifiedPost #-}

module Game.Entity.Terrain
  (Tag (..),
   Terrain,
   wall
  ) where

import Data.Color (Color)
import Data.Color qualified as Color
import Data.Glyph (Glyph (..))
import Data.Position (Position)
import Game.Behavior qualified as Behavior
import qualified Game.Flag as Flag
import GHC.Generics
import Data.Store.Exts (Store)

data Tag = Wall
  deriving stock Generic
  deriving anyclass Store

type Terrain = (Tag, Position, Glyph, Color, Behavior.Collision, Flag.Persist)

wall :: Position -> Terrain
wall p = (Wall, p, Glyph '#', Color.White, Behavior.Invalid, Flag.Persist)
