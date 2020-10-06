{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Game.Canvas where

import Data.Array (Array, array, (!), (//))
import Data.Color
import Data.Glyph
import Data.Position (Position)
import Data.Position qualified as Position

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
    up = Position.make <$> horizontal <*> pure 0
    down = Position.make <$> horizontal <*> pure size
    left = Position.make 0 <$> vertical
    right = Position.make size <$> vertical
    horizontal = [0 .. size]
    vertical = [1 .. size -1]

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
