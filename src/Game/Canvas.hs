{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Canvas where

import Data.Array (Array, array, (!), (//))
import Data.Position (Position)
import Data.Position qualified as Position
import Game.World (Color (..), Glyph (..))
import Linear

data Sprite = Sprite
  { glyph :: !Glyph,
    color :: !Color
  }
  deriving (Show)

blankSprite :: Sprite
blankSprite = Sprite (Glyph '.') White

size :: Int
size = 16

bounds :: (Position, Position)
bounds = (0 :: Position, Position.make size size)

borders :: [Position]
borders = up <> down <> left <> right
  where
    up = Position.make <$> [0..size] <*> pure 0
    down = Position.make <$> [0..size] <*> pure (size)
    left = Position.make 0 <$> [1..(size-1)]
    right = Position.make (size-1) <$> [1..(size-1)]

newtype Canvas = Canvas {unCanvas :: Array Position Sprite}
  deriving newtype (Show)

empty :: Canvas
empty = Canvas $ array bounds do
  x <- [0 .. size]
  y <- [0 .. size]
  pure (Position.make x y, blankSprite)

update :: Canvas -> [(Position, Sprite)] -> Canvas
update (Canvas arr) assocs = Canvas (arr // assocs)

-- This calls 'error' when given an invalid position.
at :: Canvas -> Position -> Sprite
at (Canvas canv) = (canv !)
