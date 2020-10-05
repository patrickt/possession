{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Canvas where

import Data.Array (Array, array, (!), (//))
import Data.Position (Position (Position), V2(V2))
import Data.Position qualified as Position
import Game.World (Color (..), Glyph (..))
import GHC.Stack (HasCallStack)

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
    horizontal = [0..size]
    vertical = [1..size-1]

newtype Canvas = Canvas {unCanvas :: Array Position Sprite}
  deriving newtype (Show)

empty :: Canvas
empty = Canvas $ array bounds do
  x <- [0 .. size]
  y <- [0 .. size]
  pure (Position.make x y, blankSprite)

update :: HasCallStack => Canvas -> [(Position, Sprite)] -> Canvas
update canv@(Canvas arr) assocs = Canvas (arr // (filter (flip contains canv . fst) assocs))

contains :: Position -> Canvas -> Bool
contains (Position (V2 x y)) _ = (x >= 0) && (x <= size) && (y >= 0) && (y <= size)

-- This calls 'error' when given an invalid position.
at :: HasCallStack => Canvas -> Position -> Sprite
at c@(Canvas arr) p
  | not (contains p c) = blankSprite
  | otherwise = arr ! p
